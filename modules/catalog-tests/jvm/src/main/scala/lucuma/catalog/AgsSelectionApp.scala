// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Functor
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.ags.*
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GmosNorthFpu
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
import lucuma.core.model.AirMassBound
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import org.http4s.jdkhttpclient.JdkHttpClient

import java.time.Duration
import java.time.Instant

trait AgsSelectionSample {

  given ADQLInterpreter = ADQLInterpreter.nTarget(100)

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

  val flamingos2Params = AgsParams.Flamingos2AgsParams(
    Flamingos2LyotWheel.F16,
    Flamingos2FpuMask.Builtin(Flamingos2Fpu.LongSlit3),
    PortDisposition.Bottom
  )

  val constraints = ConstraintSet(
    ImageQuality.Preset.PointSix,
    CloudExtinction.Preset.PointOne,
    SkyBackground.Bright,
    WaterVapor.Wet,
    ElevationRange.ByAirMass.FromBounds.get(
      AirMassBound.unsafeFromBigDecimal(BigDecimal(1.0)),
      AirMassBound.unsafeFromBigDecimal(BigDecimal(1.75))
    )
  )

  val positions = PosAngleConstraint.Unbounded
    .anglesToTestAt(
      Site.GN,
      tracking,
      now,
      Duration.ofHours(1)
    )
    .get
    .flatMap(a => offsets.map(o => AgsPosition(a, o)))

  def gaiaQuery[F[_]: Functor](gaiaClient: GaiaClient[F]): F[List[GuideStarCandidate]] =
    gaiaClient
      .queryGuideStars:
        QueryByADQL(tracking.at(now).get, candidatesArea, widestConstraints.some)
      .map:
        _.collect { case Right(t) => t }
          .map(GuideStarCandidate.siderealTarget.get)
}

object AgsSelectionSampleApp extends IOApp.Simple with AgsSelectionSample {

  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build[IO](_))
      .use(gaiaClient =>
        gaiaQuery(gaiaClient)
          .map { candidates =>
            println(s"Candidates ${candidates.length}")
            val r = Ags
              .agsAnalysis(
                constraints,
                wavelength,
                coords,
                List(coords),
                positions,
                flamingos2Params,
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
