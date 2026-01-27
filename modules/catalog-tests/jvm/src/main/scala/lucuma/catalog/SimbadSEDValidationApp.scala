// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.*
import fs2.data.csv.*
import fs2.io.readClassLoaderResource
import lucuma.catalog.clients.SimbadClient
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import lucuma.core.enums.*
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import org.http4s.jdkhttpclient.JdkHttpClient

import scala.concurrent.duration.*

object SimbadSEDValidationApp extends IOApp.Simple:

  case class SimbadEntry(
    mainId:       String,
    otype:        String,
    morphType:    String,
    spectralType: String
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

  private val testData: IO[List[SimbadEntry]] =
    readClassLoaderResource[IO]("simbad-sed-test-data-full.dat")
      .through(text.utf8.decode)
      .through(decodeUsingHeaders[SimbadEntry](' '))
      .compile
      .toList

  private val expectedOutput: IO[List[ExpectedOutput]] =
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
          .find(_.tag === spectrumTag)
          .map(UnnormalizedSED.StellarLibrary(_))

  private def stellarBaseType(sed: UnnormalizedSED): Option[String] =
    sed match
      case UnnormalizedSED.StellarLibrary(spectrum) =>
        spectrum.toString.stripSuffix("_new").some
      case _                                        =>
        none

  private def sedEquivalent(p: UnnormalizedSED, s: UnnormalizedSED): Boolean =
    (stellarBaseType(p), stellarBaseType(s)) match
      case (Some(pBase), Some(sBase)) => pBase === sBase
      case _                          => p === s

  private def extractSED(result: CatalogTargetResult): Option[UnnormalizedSED] =
    result.target.sourceProfile match
      case SourceProfile.Point(SpectralDefinition.BandNormalized(sed, _)) => sed
      case _                                                              => none

  sealed trait QueryResult
  case class Success(
    mainId:     String,
    liveSED:    Option[UnnormalizedSED],
    objectType: Option[String]
  ) extends QueryResult
  case class Failure(mainId: String, error: String) extends QueryResult

  private val mismatchesOnly = true

  private val knownMismatchIds: Set[String] = Set(
    "* alf Peg", "* del Leo", "* gam Vir",
    "2MASS J16134848-2334143",
    "BD+09  2152", "BD+17  3248",
    "CD-45  3283", "CD-80   328",
    "Feige  84", "G 139-8",
    "HD   3136", "HD  21115", "HD  21527", "HD  32255", "HD  36527",
    "HD  45207", "HD  51028", "HD  53098", "HD  66149", "HD  67288",
    "HD  71253", "HD  72766", "HD  74429", "HD  75547", "HD  76972",
    "HD  77785", "HD  78318", "HD  91651",
    "HD 102036", "HD 102392", "HD 123682", "HD 124540", "HD 135788",
    "HD 136947", "HD 137037", "HD 141441", "HD 145575", "HD 159447",
    "HD 164340", "HD 172193A", "HD 172920", "HD 174450", "HD 175798",
    "HD 187370", "HD 190336", "HD 193069A", "HD 203586", "HD 210851",
    "HD 214749", "HD 220391"
  )

  def run: IO[Unit] =
    JdkHttpClient.simple[IO].use { client =>
      for
        sedConfig <- SEDDataLoader.load
        matcher    = SEDMatcher.fromConfig(sedConfig)
        simbad     = SimbadClient.build[IO](client, matcher)
        entries   <- testData
        expected  <- expectedOutput
        filtered   = if mismatchesOnly then entries.filter(e => knownMismatchIds.contains(e.mainId))
                     else entries
        _         <- IO.println(s"Loaded ${entries.size} test entries, running ${filtered.size}" +
                       (if mismatchesOnly then " (mismatches only)" else ""))
        expectedMap = expected.map(e => e.main_id -> e).toMap
        entriesMap = entries.map(e => e.mainId -> e).toMap
        results   <- queryAll(simbad, filtered)
        _         <- reportResults(results, expectedMap, entriesMap)
      yield ()
    }

  private def queryAll(
    simbad:  SimbadClient[IO],
    entries: List[SimbadEntry]
  ): IO[List[QueryResult]] =
    val total = entries.size
    Stream
      .emits[IO, SimbadEntry](entries)
      .zipWithIndex
      .metered(100.millis)
      .evalMap { case (entry, idx) =>
        val progress = idx + 1
        val logProgress =
          if progress % 100 == 0 || progress == total.toLong then
            IO.println(s"  Progress: $progress/$total")
          else IO.unit
        querySingle(simbad, entry) <* logProgress
      }
      .compile
      .toList

  private def querySingle(
    simbad: SimbadClient[IO],
    entry:  SimbadEntry
  ): IO[QueryResult] =
    NonEmptyString.from(entry.mainId) match
      case Left(_)     =>
        IO.pure(Failure(entry.mainId, "Invalid empty main_id"))
      case Right(name) =>
        simbad
          .search(name)
          .map {
            case Right(result) =>
              val objType = result.target.catalogInfo.flatMap(_.objectType.map(_.value))
              Success(entry.mainId, extractSED(result), objType)
            case Left(errors)  =>
              Failure(entry.mainId, errors.toNonEmptyList.toList.mkString("; "))
          }
          .handleError(t => Failure(entry.mainId, t.getMessage))

  case class MismatchDetail(
    mainId:       String,
    expectedSED:  Option[UnnormalizedSED],
    liveSED:      Option[UnnormalizedSED],
    liveObjType:  Option[String],
    origOtype:    String,
    origSpType:   String,
    origMorphType: String
  )

  private def categorizeMismatch(m: MismatchDetail): String =
    (m.expectedSED, m.liveSED) match
      case (None, Some(_)) => "newly-matched"
      case _               =>
        val isStellarExpected = m.expectedSED.exists {
          case _: UnnormalizedSED.StellarLibrary => true
          case _                                 => false
        }
        val isStellarLive = m.liveSED.exists {
          case _: UnnormalizedSED.StellarLibrary => true
          case _                                 => false
        }
        if isStellarExpected && isStellarLive then "stellar-drift"
        else "galaxy-reclassification"

  private def reportResults(
    results:     List[QueryResult],
    expectedMap: Map[String, ExpectedOutput],
    entriesMap:  Map[String, SimbadEntry]
  ): IO[Unit] =
    val successes  = results.collect { case s: Success => s }
    val failures   = results.collect { case f: Failure => f }
    var matches    = 0
    var noExpected = 0
    val mismatchDetails = List.newBuilder[MismatchDetail]

    successes.foreach { case Success(mainId, liveSED, liveObjType) =>
      expectedMap.get(mainId) match
        case None =>
          noExpected += 1
        case Some(exp) =>
          val pythonSED = exp.filename.flatMap(filenameToSED)
          (pythonSED, liveSED) match
            case (None, None)                              =>
              matches += 1
            case (Some(p), Some(s)) if sedEquivalent(p, s) =>
              matches += 1
            case (p, s) if p == s                          =>
              matches += 1
            case (p, s)                                    =>
              val entry = entriesMap.get(mainId)
              mismatchDetails += MismatchDetail(
                mainId        = mainId,
                expectedSED   = p,
                liveSED       = s,
                liveObjType   = liveObjType,
                origOtype     = entry.map(_.otype).getOrElse("?"),
                origSpType    = entry.map(_.spectralType).getOrElse("?"),
                origMorphType = entry.map(_.morphType).getOrElse("?")
              )
    }

    val allMismatches = mismatchDetails.result()
    val grouped       = allMismatches.groupBy(categorizeMismatch)

    val report = new StringBuilder
    report ++= "\n=== Simbad SED Validation Report ===\n"
    report ++= s"Total entries queried: ${results.size}\n"
    report ++= s"Simbad query successes: ${successes.size}\n"
    report ++= s"Simbad query failures: ${failures.size}\n"
    report ++= s"Matches with expected: $matches\n"
    report ++= s"Mismatches: ${allMismatches.size}\n"
    report ++= s"No expected data: $noExpected\n"

    if failures.nonEmpty then
      report ++= s"\n--- First 20 query failures ---\n"
      failures.take(20).foreach { case Failure(id, err) =>
        report ++= s"  $id: $err\n"
      }

    def formatMismatch(m: MismatchDetail): String =
      val sb = new StringBuilder
      sb ++= s"  ${m.mainId}:\n"
      sb ++= s"    Original: otype=${m.origOtype}, sp_type=${m.origSpType}, morph_type=${m.origMorphType}\n"
      sb ++= s"    Live objectType: ${m.liveObjType.getOrElse("(none)")}\n"
      sb ++= s"    Expected SED: ${m.expectedSED}\n"
      sb ++= s"    Live SED:     ${m.liveSED}\n"
      sb.toString

    if allMismatches.nonEmpty then
      for (category, items) <- grouped.toList.sortBy(_._1) do
        report ++= s"\n--- ${category} (${items.size}) ---\n"
        items.sortBy(_.mainId).foreach(m => report ++= formatMismatch(m))

    report ++= "\n=== End Report ===\n"
    IO.println(report.toString)
