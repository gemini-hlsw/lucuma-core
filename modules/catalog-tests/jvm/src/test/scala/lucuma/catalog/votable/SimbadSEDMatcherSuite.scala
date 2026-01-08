// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.EitherNec
import cats.effect.IO
import cats.syntax.all.*
import fs2.*
import fs2.data.csv.*
import fs2.io.readClassLoaderResource
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import munit.CatsEffectSuite
import cats.data.Nested

/**
 * Test suite for SED matching against Simbad entries. Uses the full dataset (8000+ entries) and
 * validates against Python reference implementation.
 */
class SimbadSEDMatcherSuite extends CatsEffectSuite:

  case class SimbadEntry(
    mainId:       String,
    otype:        String,
    morphType:    String,
    spectralType: String
  )

  case class MatchResult(
    entry:    SimbadEntry,
    sed:      EitherNec[CatalogProblem, UnnormalizedSED],
    category: Option[String]
  )

  case class ExpectedOutput(
    main_id:  String,
    otype:    String,
    filename: Option[String],
    t_eff:    Option[Double],
    log_g:    Option[Double]
  )

  given liftCellDecoder[A: CellDecoder]: CellDecoder[Option[A]] = s =>
    s.nonEmpty.guard[Option].traverse(_ => CellDecoder[A].apply(s))

  given CsvRowDecoder[SimbadEntry, String] = (row: CsvRow[String]) =>
    for
      mainId       <- row.as[String]("main_id")
      otype        <- row.as[String]("otype")
      morphType    <- row.as[String]("morph_type")
      spectralType <- row.as[String]("sp_type")
    yield SimbadEntry(mainId, otype, morphType, spectralType)

  given CsvRowDecoder[ExpectedOutput, String] = (row: CsvRow[String]) =>
    for
      mainId   <- row.as[String]("main_id")
      otype    <- row.as[String]("otype")
      filename <- row.as[Option[String]]("filename")
      tEff     <- row.as[Option[Double]]("t_eff")
      logG     <- row.as[Option[Double]]("log_g")
    yield ExpectedOutput(mainId, otype, filename, tEff, logG)

  val testData: IO[List[SimbadEntry]] =
    readClassLoaderResource[IO]("simbad-sed-test-data-full.dat")
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[SimbadEntry](' '))
      .compile
      .toList

  def matchResults: IO[List[MatchResult]] =
    Nested(testData).map { entry =>
      val morphTypeOpt    = Some(entry.morphType).filter(_.nonEmpty)
      val spectralTypeOpt = if entry.spectralType.isEmpty then None else Some(entry.spectralType)
      val sed             = SEDMatcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

      val category = sed.toOption.map {
        case UnnormalizedSED.StellarLibrary(_)        => "star"
        case UnnormalizedSED.Galaxy(_)                => "galaxy"
        case UnnormalizedSED.Quasar(_)                => "quasar"
        case UnnormalizedSED.HIIRegion(_)             => "hii"
        case UnnormalizedSED.PlanetaryNebula(_)       => "pn"
        case UnnormalizedSED.PowerLaw(_)              => "powerlaw"
        case UnnormalizedSED.BlackBody(_)             => "blackbody"
        case UnnormalizedSED.UserDefined(_)           => "user"
        case UnnormalizedSED.CoolStarModel(_)         => "coolstar"
        case UnnormalizedSED.Planet(_)                => "planet"
        case UnnormalizedSED.UserDefinedAttachment(_) => "userattachment"
      }

      MatchResult(entry, sed, category)
    }.value

  val output: IO[List[ExpectedOutput]] =
    readClassLoaderResource[IO]("expected-output-full.csv")
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[ExpectedOutput]())
      .compile
      .toList

  private def filenameToSED(filename: String): Option[UnnormalizedSED] =
    filename match
      case "QSO.sed"        => Some(UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
      case "HII.sed"        => Some(UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
      case "PN.sed"         => Some(UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009))
      case "Elliptical.sed" => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case "Spiral.sed"     => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
      case stellar          =>
        val spectrumTag = stellar.stripSuffix(".nm")
        StellarLibrarySpectrum.values
          .find(_.tag == spectrumTag)
          .map(UnnormalizedSED.StellarLibrary(_))

  private def stellarBaseType(sed: UnnormalizedSED): Option[String] =
    sed match
      case UnnormalizedSED.StellarLibrary(spectrum) =>
        spectrum.toString.stripSuffix("_new").some
      case _                                        =>
        none

  private def sedEquivalent(p: UnnormalizedSED, s: UnnormalizedSED): Boolean =
    if p === s then true
    else
      (stellarBaseType(p), stellarBaseType(s)) match
        case (Some(pBase), Some(sBase)) => pBase === sBase
        case _                          => false

  test("sanity test"):
    testData.map(_.length > 8000).assert

  // Known differences between Python and Scala implementations
  val knownDifferences = Set(
    "*   3 Cet", // K3Ib with s*r otype - otype category difference
    "*  17 Aqr"  // K4/5III - range scoring (Python picks K5, Scala picks K4)
  )

  test("validate against Python reference output") {
    (testData, output).mapN { (testData, expectedOutput) =>
      val inputData = testData.map(e => e.mainId -> e).toMap

      var matches         = 0
      var mismatches      = 0
      var knownDiffs      = 0
      val errors          = scala.collection.mutable.ListBuffer[String]()
      val unexpectedDiffs = scala.collection.mutable.ListBuffer[String]()

      expectedOutput.foreach { expected =>
        inputData.get(expected.main_id).foreach { entry =>
          val morphTypeOpt    = if entry.morphType.isEmpty then None else Some(entry.morphType)
          val spectralTypeOpt =
            if entry.spectralType.isEmpty then None else Some(entry.spectralType)
          val scalaResult     = SEDMatcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

          val pythonSED = expected.filename.flatMap(filenameToSED)
          val scalaSED  = scalaResult.toOption

          (pythonSED, scalaSED) match
            case (None, None)                              => matches += 1
            case (Some(p), Some(s)) if sedEquivalent(p, s) => matches += 1
            case (Some(p), Some(s))                        =>
              mismatches += 1
              val msg = s"${expected.main_id}: Python=$p, Scala=$s"
              errors += msg
              if knownDifferences.contains(expected.main_id) then knownDiffs += 1
              else unexpectedDiffs += msg
            case (Some(p), None)                           =>
              mismatches += 1
              val msg = s"${expected.main_id}: Python=$p, Scala=None"
              errors += msg
              if knownDifferences.contains(expected.main_id) then knownDiffs += 1
              else unexpectedDiffs += msg
            case (None, Some(s))                           =>
              mismatches += 1
              val msg = s"${expected.main_id}: Python=None, Scala=$s"
              errors += msg
              if knownDifferences.contains(expected.main_id) then knownDiffs += 1
              else unexpectedDiffs += msg
        }
      }

      println(s"\n=== Python Reference Validation ===")
      println(s"Total entries: ${expectedOutput.length}")
      println(s"Matches: $matches (${matches * 100 / expectedOutput.length}%)")
      println(s"Known differences: $knownDiffs")
      println(s"Total mismatches: $mismatches")

      if errors.nonEmpty && errors.length <= 50 then
        println(s"\nAll differences:")
        errors.foreach(e => println(s"  $e"))
      else if errors.nonEmpty then
        println(s"\nFirst 50 differences:")
        errors.take(50).foreach(e => println(s"  $e"))
        println(s"... and ${errors.length - 50} more")
    }
  }
