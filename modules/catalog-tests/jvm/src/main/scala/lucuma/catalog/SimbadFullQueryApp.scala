// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.*
import lucuma.catalog.SimbadData.*
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

// test app to validate SED matching querying simbad directly
object SimbadFullQueryApp extends IOApp.Simple:

  private def retry[A](
    io:               IO[A],
    retriesRemaining: Int,
    interval:         FiniteDuration
  ): IO[A] =
    io.handleErrorWith { err =>
      if retriesRemaining > 0 then
        IO.sleep(interval) *>
          retry(io, retriesRemaining - 1, interval * 2)
      else IO.raiseError(err)
    }

  private def extractSED(result: CatalogTargetResult): Option[UnnormalizedSED] =
    result.target.sourceProfile match
      case SourceProfile.Point(SpectralDefinition.BandNormalized(sed, _)) => sed
      case _                                                              => none

  sealed trait QueryResult:
    def isSuccess: Boolean = this match
      case _: Success => true
      case _          => false

  case class Success(
    mainId:     String,
    liveSED:    Option[UnnormalizedSED],
    objectType: Option[String]
  ) extends QueryResult
  case class Failure(mainId: String, error: String) extends QueryResult

  override def run: IO[Unit] =
    JdkHttpClient.simple[IO].use { client =>
      for {
        loader     <- SEDDataLoader.load
        matcher     = SEDMatcher.fromConfig(loader)
        physics     = new StellarPhysics(loader.gravityTable)
        library     = new StellarLibraryParameters(loader.stellarLibrary, physics)
        simbad      = SimbadClient.build[IO](client, matcher)
        entries    <- testData
        expected   <- expectedOutput
        _          <- IO.println(s"Loaded ${entries.size} test entries")
        expectedMap = expected.fproductLeft(_.main_id).toMap
        results    <- queryAll(simbad, entries)
        _          <- reportResults(results, expectedMap)
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
      // Let's not hammer Simbad
      .metered(20.millis)
      .parEvalMap(5):
        case (entry, idx) =>
          val progress    = idx + 1
          val logProgress =
            if progress % 100 === 0 || progress === total.toLong then
              IO.println(s"Progress: $progress/$total")
            else IO.unit
          queryOne(simbad, entry) <* logProgress
      .compile
      .toList

  private def queryOne(
    simbad: SimbadClient[IO],
    entry:  SimbadEntry
  ): IO[QueryResult] =
    NonEmptyString.from(entry.mainId) match
      case Left(_)     =>
        IO.pure(Failure(entry.mainId, "Invalid empty main_id"))
      case Right(name) =>
        val attempt = simbad
          .search(name)
          .map:
            case Right(result) =>
              val objType = result.target.catalogInfo.flatMap(_.objectType.map(_.value))
              Success(entry.mainId, extractSED(result), objType)

            case Left(errors) =>
              Failure(entry.mainId, errors.toNonEmptyList.toList.mkString("; "))

        retry(attempt, 5, 100.millis)
          .handleError(t => Failure(entry.mainId, t.getMessage))

  private def reportResults(
    results:     List[QueryResult],
    expectedMap: Map[String, ExpectedOutput]
  ): IO[Unit] =
    val (successes, failures) = results.partition(_.isSuccess)

    val (matches, mismatches, unexpected) = successes.foldLeft((0, 0, 0)):
      case ((m, mm, ne), Success(mainId, liveSED, _)) =>
        expectedMap.get(mainId) match
          case None      =>
            (m, mm, ne + 1)
          case Some(exp) =>
            val pythonSED = exp.filename.flatMap(filenameToSED)
            (pythonSED, liveSED) match
              case (None, None)                              =>
                (m + 1, mm, ne)
              case (Some(p), Some(s)) if sedEquivalent(p, s) =>
                (m + 1, mm, ne)
              case (p, s) if p == s                          =>
                (m + 1, mm, ne)
              case _                                         =>
                (m, mm + 1, ne)
      case (acc, _)                                   => acc

    val report =
      s"""=== Simbad SED Validation Report ===
        |
        | Total entries : ${results.size}
        | Successes: ${successes.size}
        | Failures: ${failures.size}
        | Matches: $matches
        | Mismatches: $mismatches
        | No expected data: $unexpected""".stripMargin

    IO.println(report)
