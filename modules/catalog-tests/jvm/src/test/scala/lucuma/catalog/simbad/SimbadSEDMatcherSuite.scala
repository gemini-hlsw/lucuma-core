// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.data.EitherNec
import cats.data.Nested
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import fs2.*
import fs2.data.csv.*
import fs2.io.readClassLoaderResource
import lucuma.catalog.votable.CatalogProblem
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import munit.CatsEffectSuite

/**
 * Test suite for SED matching against Simbad entries. Uses the full dataset (8000+ entries) and
 * validates against Python reference implementation.
 */
class SimbadSEDMatcherSuite extends CatsEffectSuite:

  case class SEDTestFixture(
    matcher: SEDMatcher,
    library: StellarLibraryParameters,
    physics: StellarPhysics
  )

  val sedFixture = ResourceSuiteLocalFixture(
    "sedFixture",
    Resource.eval(SEDDataLoader.load.map { sedConfig =>
      val physics = new StellarPhysics(sedConfig.gravityTable)
      val library = new StellarLibraryParameters(sedConfig.stellarLibrary, physics)
      val matcher = new SEDMatcher(library, physics)
      SEDTestFixture(matcher, library, physics)
    })
  )

  override def munitFixtures = List(sedFixture)

  def matcher: SEDMatcher               = sedFixture().matcher
  def library: StellarLibraryParameters = sedFixture().library
  def physics: StellarPhysics           = sedFixture().physics

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
      val sed             = matcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

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

  private def isPerfectTie(
    pythonSED: UnnormalizedSED,
    scalaSED:  UnnormalizedSED,
    entry:     SimbadEntry
  ): Boolean =
    (pythonSED, scalaSED) match
      case (UnnormalizedSED.StellarLibrary(pSpectrum), UnnormalizedSED.StellarLibrary(sSpectrum)) =>
        val cleaned = entry.spectralType.replaceAll("[():]", "")
        SpectralTypeParsers.spectralType
          .parse(cleaned)
          .toOption
          .flatMap { case (_, (lum, temp)) =>
            physics.calculateParameters(lum, temp).flatMap { targetParams =>
              val pParams = library.params.get(pSpectrum)
              val sParams = library.params.get(sSpectrum)

              (pParams, sParams).mapN { (pp, sp) =>
                val dtMax = 0.1 * targetParams.temp.value
                val dgMax = 0.5

                def score(libParams: StellarPhysics.StellarParameters): Double =
                  val dt = (libParams.temp.value - targetParams.temp.value).toDouble / dtMax
                  val dg = (libParams.logG - targetParams.logG) / dgMax
                  math.sqrt(dt * dt + dg * dg)

                val pScore = score(pp)
                val sScore = score(sp)

                math.abs(pScore - sScore) / math.max(pScore, sScore) < 0.01
              }
            }
          }
          .getOrElse(false)
      case _                                                                                      => false

  test("sanity test"):
    testData.map(_.length > 8000).assert

  test("validate against Python") {
    (testData, output).mapN { (testData, expectedOutput) =>
      val inputData = testData.map(e => e.mainId -> e).toMap

      case class ValidationResult(
        matches: Int = 0,
        ties:    Int = 0,
        errors:  List[String] = Nil
      )

      val result = expectedOutput.foldLeft(ValidationResult()) { (acc, expected) =>
        inputData.get(expected.main_id) match
          case None        => acc
          case Some(entry) =>
            val morphTypeOpt    = if entry.morphType.isEmpty then None else Some(entry.morphType)
            val spectralTypeOpt =
              if entry.spectralType.isEmpty then None else Some(entry.spectralType)
            val scalaResult     = matcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

            val pythonSED = expected.filename.flatMap(filenameToSED)
            val scalaSED  = scalaResult.toOption

            (pythonSED, scalaSED) match
              case (None, None)                                    =>
                acc.copy(matches = acc.matches + 1)
              case (Some(p), Some(s)) if sedEquivalent(p, s)       =>
                acc.copy(matches = acc.matches + 1)
              case (Some(p), Some(s)) if isPerfectTie(p, s, entry) =>
                acc.copy(matches = acc.matches + 1, ties = acc.ties + 1)
              case (Some(p), Some(s))                              =>
                acc.copy(errors =
                  s"${entry.mainId} (${entry.spectralType}): Python=${p}, Scala=${s}" :: acc.errors
                )
              case (Some(p), None)                                 =>
                acc.copy(errors =
                  s"${entry.mainId}: Python matched ($p), Scala failed" :: acc.errors
                )
              case (None, Some(s))                                 =>
                acc.copy(errors =
                  s"${entry.mainId}: Python failed, Scala matched ($s)" :: acc.errors
                )
      }

      assertEquals(
        result.errors,
        Nil,
        s"Found ${result.errors.length} mismatches"
      )
      assert(result.matches > 8000, s"Expected > 8000 matches, got ${result.matches}")
    }
  }
