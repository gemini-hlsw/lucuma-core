// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.pwfs.patrolField
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.SiderealTracking
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

import java.time.Instant

object ShortCut9024App extends IOApp.Simple {

  given LoggerFactory[IO] = NoOpFactory[IO]
  given ADQLInterpreter   = ADQLInterpreter.nTarget(100)

  // Gaia DR3 source for the offending guide star.
  private val OffendingId = 5145821452872321792L

  private val now = Instant.parse("2026-06-28T21:05:00Z")

  private val pm  =
    ProperMotion(
      ProperMotion.RA.milliarcsecondsPerYear.reverseGet(BigDecimal(12.139)),
      ProperMotion.Dec.milliarcsecondsPerYear.reverseGet(BigDecimal(-11.849))
    )
  private val plx = Parallax.milliarcseconds.reverseGet(BigDecimal(3.075))

  // A science target at the given RA/Dec (epoch J2000), pm-corrected
  private def target(ra: String, dec: String): Coordinates =
    SiderealTracking(
      (RightAscension.fromStringHMS.getOption(ra), Declination.fromStringSignedDMS.getOption(dec))
        .mapN(Coordinates.apply)
        .get,
      Epoch.J2000,
      pm.some,
      RadialVelocity.Zero.some,
      plx.some
    ).at(now).get

  private val t1   = target("02:27:53.329", "-15:15:30.76")
  private val t2   = target("02:27:32.329", "-15:14:28.76")
  private val base = Coordinates.centerOf(NonEmptyList.of(t1, t2))

  private val constraints = ConstraintSet(
    ImageQuality.Preset.OnePointZero,
    CloudExtinction.Preset.PointThree,
    SkyBackground.Bright,
    WaterVapor.Wet,
    ElevationRange.ByAirMass.Default
  )
  private val wavelength  = Wavelength.fromIntNanometers(900).get

  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build[IO](_))
      .use: gaiaClient =>
        gaiaClient
          .queryGuideStars(
            QueryByADQL(base, patrolField.patrolField, widestConstraints.some, DefaultAreaBuffer)
          )
          .map(_.collect { case Right(t) =>
            t
          }.map(GuideStarCandidate.siderealTarget.get(_).at(now)))
          .map { candidates =>
            println(s"obs instant      : $now")
            println(s"base             : $base")
            println(s"target 1 / 2     : $t1  /  $t2")
            println(s"gaia candidates  : ${candidates.length}")

            val r        = Ags.agsAnalysis(
              constraints,
              wavelength,
              base,
              List(t1, t2),
              None,
              NonEmptyList.of(Angle.Angle0),
              None,
              None,
              AgsParams.GhostIfu(PortDisposition.Bottom),
              candidates
            )
            val analyses = r.analyses

            // Verdict for the known offending star.
            val offending = analyses.filter(_.target.id === OffendingId)
            println(
              s"offending star $OffendingId verdict(s): ${offending.map(Ags.resultLabel).mkString(", ")}"
            )

            // The selected (best-ranked) guide star — what Explore would pick.
            val ranked = analyses.sortUsablePositions
            ranked.headOption match
              case None       => println("selected         : <none usable>")
              case Some(best) =>
                println(
                  s"selected         : id=${best.target.id} vignetting=${best.vignetting.toMicroarcsecondsSquared} µas² posAngle=${best.posAngle.toDoubleDegrees}"
                )
                println(s"selected == offending? ${best.target.id === OffendingId}")

            println("--- top usable (id, vignetting µas²) ---")
            ranked
              .take(5)
              .foreach(u => println(s"  ${u.target.id}  ${u.vignetting.toMicroarcsecondsSquared}"))
          }
      .void
}
