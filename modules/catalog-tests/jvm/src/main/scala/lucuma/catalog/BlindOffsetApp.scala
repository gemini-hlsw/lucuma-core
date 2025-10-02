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
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SiderealTracking
import org.http4s.jdkhttpclient.JdkHttpClient

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

trait BlindOffsetSample:

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
    val objectTracking   = ObjectTracking.SiderealObjectTracking(siderealTracking)
    BlindOffsets.runBlindOffsetAnalysis(gaiaClient, objectTracking, observationTime)
  }

  def printCandidates(candidates: List[BlindOffsetCandidate]): IO[Unit] = IO {
    println(s"Found ${candidates.length} blind offset star candidates, sorted by score:")
    println("rank, sourceId, gmag, distance, score")

    candidates.zipWithIndex.foreach { case (candidate, index) =>
      val rank     = index + 1
      val sourceId = candidate.sourceId
      val gMag     = BlindOffsetCandidate.referenceBrightness(candidate.catalogResult) match {
        case Some(mag) => f"${mag}%5.2f"
        case None      => " N/A"
      }
      val distance = f"${Angle.decimalArcseconds.get(candidate.distance)}%3.1f"
      val score    = f"${candidate.score}%3.3f"
      println(f"$rank%4d, $sourceId%18s, $gMag%5s, ${distance} arcsec, $score%6s")
    }

  }

object BlindOffsetApp extends IOApp.Simple with BlindOffsetSample:

  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build[IO](_, adapters = NonEmptyChain.of[Gaia](Gaia3LiteGavo)))
      .use: gaiaClient =>
        for {
          _          <- IO.println(s"Querying blind offset star candidates on: $coords")
          candidates <- runBlindOffsets(gaiaClient, coords)
          _          <- printCandidates(candidates)
        } yield ()
