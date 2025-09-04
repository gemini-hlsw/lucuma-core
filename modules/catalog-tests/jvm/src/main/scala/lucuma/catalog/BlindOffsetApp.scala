// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.CatalogAdapter.*
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

trait BlindOffsetSample {

  // Use a fixed observation time for consistent results (today at midnight UTC)
  protected val observationTime: Instant = LocalDate.now().atStartOfDay(ZoneOffset.UTC).toInstant()

  // Test coordinates near Pleiades (same as in our test data)
  val coords = (
    RightAscension.fromStringHMS.getOption("05:35:17.3"),
    Declination.fromStringSignedDMS.getOption("+22:00:52.0")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  // Alternative test coordinates (M81 galaxy region)
  val m81Coords = (
    RightAscension.fromStringHMS.getOption("09:55:33.2"),
    Declination.fromStringSignedDMS.getOption("+69:03:55.0")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  def blindOffsetQuery(gaiaClient: GaiaClient[IO]): IO[List[BlindOffsetCandidate]] = {
    val siderealTracking = SiderealTracking(
      baseCoordinates = coords,
      epoch = Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    )
    val objectTracking   = ObjectTracking.SiderealObjectTracking(siderealTracking)
    gaiaClient.blindOffsetCandidates(objectTracking, observationTime)
  }

  def printCandidates(candidates: List[BlindOffsetCandidate]): IO[Unit] = IO {
    println(s"Found ${candidates.length} blind offset star candidates:")
    println("=" * 80)
    println(f"${"Rank"}%4s | ${"Source ID"}%18s | ${"G Mag"}%5s | ${"Distance"}%8s | ${"Score"}%6s")
    println("-" * 80)

    candidates.zipWithIndex.foreach { case (candidate, index) =>
      val rank     = index + 1
      val sourceId = candidate.sourceId
      val gMag     = BlindOffsetCandidate.extractGMagnitude(candidate.target) match {
        case Some(mag) => f"${mag}%5.2f"
        case None      => " N/A"
      }
      val distance = f"${candidate.angularDistance.toMicroarcseconds / 1000000.0}%8.1f"
      val score    = f"${candidate.score}%6.3f"
      println(f"$rank%4d | $sourceId%18s | $gMag%5s | ${distance}arcsec | $score%6s")
    }

    if (candidates.nonEmpty) {
      val best     = candidates.head
      println()
      println(s"Best candidate: ${best.sourceId}")
      val bestGMag = BlindOffsetCandidate.extractGMagnitude(best.target) match {
        case Some(mag) => mag.toString
        case None      => "N/A"
      }
      println(s"  G magnitude: $bestGMag")
      println(s"  Distance: ${best.angularDistance.toMicroarcseconds / 1000000.0} arcseconds")
      println(s"  Score: ${best.score}")
      println(s"  Coordinates: ${best.coordinates}")
    }
  }
}

object BlindOffsetApp extends IOApp.Simple with BlindOffsetSample {

  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build[IO](_, adapters = NonEmptyChain.of[Gaia](Gaia3LiteGavo)))
      .use { gaiaClient =>
        for {
          _ <- IO.println(s"Querying blind offset star candidates around coordinates: $coords")
          _ <- IO.println("This will search for stars within 180 arcseconds with G magnitude > 12")
          _ <- IO.println("")

          candidates <- blindOffsetQuery(gaiaClient)
          _          <- printCandidates(candidates)

          _ <- IO.println("")
          _ <- IO.println("=" * 80)
          _ <- IO.println("Testing with alternative coordinates (M81 region):")

          m81SiderealTracking    = SiderealTracking(
                                     baseCoordinates = m81Coords,
                                     epoch = Epoch.J2000,
                                     properMotion = None,
                                     radialVelocity = None,
                                     parallax = None
                                   )
          m81ObjectTracking      = ObjectTracking.SiderealObjectTracking(m81SiderealTracking)
          alternativeCandidates <-
            gaiaClient.blindOffsetCandidates(m81ObjectTracking, observationTime)
          _                     <- IO.println(s"Found ${alternativeCandidates.length} candidates around M81 region")

          // Show just the top 5 for the alternative region
          _ <- if (alternativeCandidates.nonEmpty) {
                 val top5 = alternativeCandidates.take(5)
                 IO.println("Top 5 candidates:") *>
                   printCandidates(top5)
               } else {
                 IO.println("No candidates found in M81 region")
               }

        } yield ()
      }
}
