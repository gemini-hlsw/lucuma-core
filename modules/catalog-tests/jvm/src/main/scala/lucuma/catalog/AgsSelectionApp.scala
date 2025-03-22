// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.syntax.all.*
import fs2.*
import lucuma.ags.*
import lucuma.catalog.votable.*
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.F2LyotWheel
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.geom.gmos.all.candidatesArea
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.ElevationRange.AirMass.DecimalValue
import lucuma.core.model.ObjectTracking
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.sequence.f2.F2FpuMask
import org.http4s.Method.*
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.jdkhttpclient.JdkHttpClient

import java.time.Duration
import java.time.Instant

trait AgsSelectionSample {

  given gaia: CatalogAdapter.Gaia = CatalogAdapter.Gaia3Lite

  given ADQLInterpreter =
    ADQLInterpreter.nTarget(100)

  val coords = (RightAscension.fromStringHMS.getOption("19:59:36.748"),
                Declination.fromStringSignedDMS.getOption("+20:48:14.60")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  val tracking = SiderealTracking(
    coords,
    Epoch.J2000,
    ProperMotion(ProperMotion.RA.milliarcsecondsPerYear.reverseGet(-16),
                 ProperMotion.Dec.milliarcsecondsPerYear.reverseGet(-26)
    ).some,
    RadialVelocity.Zero.some,
    none
  )

  val now        = Instant.ofEpochMilli(1688486539L)
  val wavelength = Wavelength.fromIntNanometers(600).get

  val offsets =
    NonEmptyList.of(Offset.Zero, Offset.Zero.copy(q = Offset.Q(Angle.fromDoubleArcseconds(15))))

  val gmosParams = AgsParams.GmosAgsParams(
    GmosNorthFpu.LongSlit_0_25.asLeft.some,
    PortDisposition.Bottom
  )

  val f2Params = AgsParams.F2AgsParams(
    F2LyotWheel.F16,
    F2FpuMask.Builtin(F2Fpu.LongSlit3),
    PortDisposition.Bottom
  )

  val constraints = ConstraintSet(
    ImageQuality.PointSix,
    CloudExtinction.PointOne,
    SkyBackground.Bright,
    WaterVapor.Wet,
    AirMass.fromDecimalValues.get(DecimalValue.unsafeFrom(BigDecimal(1.0)),
                                  DecimalValue.unsafeFrom(BigDecimal(1.75))
    )
  )

  val positions = PosAngleConstraint.Unbounded
    .anglesToTestAt(
      Site.GN,
      ObjectTracking.SiderealObjectTracking(tracking),
      now,
      Duration.ofHours(1)
    )
    .get
    .flatMap(a => offsets.map(o => AgsPosition(a, o)))

  def gaiaQuery[F[_]: Sync](client: Client[F]): Stream[F, Target.Sidereal] = {
    val query   =
      CatalogSearch.gaiaSearchUri(
        QueryByADQL(tracking.at(now).get, candidatesArea, widestConstraints.some)
      )
    val request = Request[F](GET, query)
    client
      .stream(request)
      .flatMap(
        _.body
          .through(text.utf8.decode)
          // .evalTap(a => Sync[F].delay(println(a)))
          .through(CatalogSearch.guideStars[F](gaia))
          .collect { case Right(t) => t }
          // .evalTap(a => Sync[F].delay(println(a)))
      )
  }
}

object AgsSelectionSampleApp extends IOApp.Simple with AgsSelectionSample {

  def run =
    JdkHttpClient
      .simple[IO]
      .flatMap(
        gaiaQuery[IO](_)
          .map(GuideStarCandidate.siderealTarget.get)
          .compile
          .toList
          .map { candidates =>
            println(s"Candidates ${candidates.length}")
            val r = Ags
              .agsAnalysis(
                constraints,
                wavelength,
                coords,
                List(coords),
                positions,
                f2Params,
                candidates
              )
            pprint.pprintln(r.sortUsablePositions)
            r.sortUsablePositions
          }
      )
      .flatTap(x => IO.println(x.length))
      // .flatMap(x => x.filter(_.isUsable).traverse(u => IO(pprint.pprintln(u))))
      .void
}
