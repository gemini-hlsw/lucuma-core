// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.CatalogAdapter.*
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.*
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.http4s.client.middleware.{ResponseLogger => CliLogger}

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

trait BlindOffsetSample:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]
  given L: Logger[IO]     = LoggerFactory[IO].getLoggerFromName("blind-offset-test")

  protected val observationTime: Instant =
    LocalDate.of(2025, 9, 4).atStartOfDay(ZoneOffset.UTC).toInstant()

  val coords = (
    RightAscension.fromStringHMS.getOption("05:35:17.3"),
    Declination.fromStringSignedDMS.getOption("+22:00:52.0")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  def runBlindOffsets(
    gaiaClient:  GaiaClient[IO],
    coordinates: Coordinates
  ): IO[List[BlindOffsetCandidate]] = {
    val siderealTracking = SiderealTracking(
      baseCoordinates = coordinates,
      epoch = Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    )
    BlindOffsets.runBlindOffsetAnalysis(gaiaClient, siderealTracking, observationTime)
  }

  def printCandidates(candidates: List[BlindOffsetCandidate]): IO[Unit] =
    L.info(s"Found ${candidates.length} blind offset star candidates, sorted by score:") *>
      L.info(f"rank, ${"sourceId"}%28s,  gmag, ${"distance"}%4s, score") *>
      candidates.zipWithIndex.traverse_ : (candidate, index) =>
        val rank     = index + 1
        val sourceId = candidate.sourceId
        val gMag     = BlindOffsetCandidate.referenceBrightness(candidate.catalogResult) match {
          case Some(mag) => f"${mag}%5.2f"
          case None      => " N/A"
        }
        val distance = f"${Angle.decimalArcseconds.get(candidate.distance)}%3.1f"
        val score    = f"${candidate.score}%3.3f"
        val msg      = f"$rank%4d, $sourceId%18s, $gMag%5s, ${distance} arcsec, $score%6s"
        L.info(msg)

object BlindOffsetApp extends IOApp.Simple with BlindOffsetSample:

  val loggerClient = CliLogger[IO](
    logHeaders = true,
    logBody = false
  )

  def run =
    JdkHttpClient
      .simple[IO]
      .map(c =>
        GaiaClient.build[IO](
          loggerClient(c),
          adapters = NonEmptyChain.of[Gaia](Gaia3EsaProxy)
        )
      )
      .use: gaiaClient =>
        for {
          _          <- IO.println(s"Querying blind offset star candidates on: $coords")
          candidates <- runBlindOffsets(gaiaClient, coords)
          _          <- printCandidates(candidates)
        } yield ()
