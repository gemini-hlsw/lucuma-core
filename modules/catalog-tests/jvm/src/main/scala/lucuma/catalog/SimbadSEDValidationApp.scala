// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.*
import lucuma.catalog.SEDValidationData.*
import lucuma.catalog.clients.SimbadClient
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import lucuma.catalog.simbad.StellarLibraryParameters
import lucuma.catalog.simbad.StellarPhysics
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import org.http4s.jdkhttpclient.JdkHttpClient

import scala.concurrent.duration.*

object SimbadSEDValidationApp extends IOApp.Simple:

  private def retry[A](io: IO[A], maxRetries: Int, delay: FiniteDuration): IO[A] =
    io.handleErrorWith { err =>
      if maxRetries > 0 then IO.sleep(delay) *> retry(io, maxRetries - 1, delay * 2)
      else IO.raiseError(err)
    }

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

  def run: IO[Unit] =
    JdkHttpClient.simple[IO].use { client =>
      for {
        sedConfig <- SEDDataLoader.load
        matcher     = SEDMatcher.fromConfig(sedConfig)
        physics     = new StellarPhysics(sedConfig.gravityTable)
        library     = new StellarLibraryParameters(sedConfig.stellarLibrary, physics)
        simbad      = SimbadClient.build[IO](client, matcher)
        entries    <- testData
        expected   <- expectedOutput
        _          <- IO.println(s"Loaded ${entries.size} test entries")
        expectedMap = expected.map(e => e.main_id -> e).toMap
        entriesMap  = entries.map(e => e.mainId -> e).toMap
        results    <- queryAll(simbad, entries)
        _          <- reportResults(results, expectedMap, entriesMap, physics, library)
      } yield ()
    }

  private def queryAll(
    simbad:  SimbadClient[IO],
    entries: List[SimbadEntry]
  ): IO[List[QueryResult]] =
    val total = entries.size
    Stream
      .emits[IO, SimbadEntry](entries)
      .zipWithIndex
      .metered(50.millis)
      .parEvalMap(5) { case (entry, idx) =>
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
        val attempt = simbad
          .search(name)
          .map {
            case Right(result) =>
              val objType = result.target.catalogInfo.flatMap(_.objectType.map(_.value))
              Success(entry.mainId, extractSED(result), objType)
            case Left(errors)  =>
              Failure(entry.mainId, errors.toNonEmptyList.toList.mkString("; "))
          }
        retry(attempt, 3, 1.second)
          .handleError(t => Failure(entry.mainId, t.getMessage))

  case class MismatchDetail(
    mainId:        String,
    expectedSED:   Option[UnnormalizedSED],
    liveSED:       Option[UnnormalizedSED],
    liveObjType:   Option[String],
    origOtype:     String,
    origSpType:    String,
    origMorphType: String
  )

  private def isMismatchScoreTie(
    m:       MismatchDetail,
    physics: StellarPhysics,
    library: StellarLibraryParameters
  ): Boolean =
    (m.expectedSED, m.liveSED) match
      case (Some(expected), Some(live)) =>
        isScoreTie(m.origSpType, expected, live, physics, library)
      case _ => false

  private def categorizeMismatch(
    m:       MismatchDetail,
    physics: StellarPhysics,
    library: StellarLibraryParameters
  ): String =
    (m.expectedSED, m.liveSED) match
      case (None, Some(_)) => "newly-matched"
      case _ if isMismatchScoreTie(m, physics, library) => "score-tie"
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
    entriesMap:  Map[String, SimbadEntry],
    physics:     StellarPhysics,
    library:     StellarLibraryParameters
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
    val grouped       = allMismatches.groupBy(m => categorizeMismatch(m, physics, library))

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
