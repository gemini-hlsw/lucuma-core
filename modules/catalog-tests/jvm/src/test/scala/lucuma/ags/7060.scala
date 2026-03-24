// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyChain
import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.AgsAnalysis.Usable
import lucuma.ags.ScienceOffsets
import lucuma.ags.syntax.*
import lucuma.catalog.clients.GaiaClientMock
import lucuma.catalog.votable.*
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.ProperMotion
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.AirMassBound
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.SiderealTracking
import lucuma.core.syntax.all.*
import munit.CatsEffectSuite
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

import java.time.Instant

// This is a replication of what explore does for the sample observation
// It does not fix it
// https://app.shortcut.com/lucuma/story/7060/gpp-ags-does-not-match-ocs-ags
class ShortCut_7060 extends CatsEffectSuite:

  given LoggerFactory[IO] = NoOpFactory[IO]
  given ADQLInterpreter   = ADQLInterpreter.nTarget(100)

  // Target coordinates for GP221000-483213
  // made to match
  // https://explore-dev.lucuma.xyz/p-d34/observation/o-3cf6/target/t-13784
  val targetCoords = Coordinates(
    RightAscension.fromDoubleDegrees(317.50417),
    Declination.fromDoubleDegrees(-48.53700).getOrElse(Declination.Zero)
  )

  val wavelength = Wavelength.fromIntNanometers(750).get

  val tracking = SiderealTracking(
    targetCoords,
    Epoch.J2000,
    ProperMotion.Zero.some,
    Redshift(0.4).toRadialVelocity,
    None
  )

  val now = Instant.parse("2025-09-30T22:39:00Z")

  val acqOffsets =
    AcquisitionOffsets(
      NonEmptySet.of(
        Offset.Zero.guided,
        Offset.Zero.copy(p = Offset.P(Angle.fromDoubleArcseconds(10))).guided
      )
    )
  val sciOffsets =
    ScienceOffsets(
      NonEmptySet.of(
        Offset.Zero.copy(q = Offset.Q(Angle.fromDoubleArcseconds(-15))).guided,
        Offset.Zero.guided,
        Offset.Zero.copy(q = Offset.Q(Angle.fromDoubleArcseconds(15))).guided
      )
    )

  val anglesToTest = PosAngleConstraint.AverageParallactic
    .anglesToTestAt(
      Angle.fromDoubleDegrees(300.96).some
    )
    .get

  val constraints = ConstraintSet(
    ImageQuality.Preset.OnePointZero,
    CloudExtinction.Preset.PointThree,
    SkyBackground.Gray,
    WaterVapor.Wet,
    ElevationRange.ByAirMass.FromBounds.get(
      AirMassBound.unsafeFromBigDecimal(BigDecimal(1.0)),
      AirMassBound.unsafeFromBigDecimal(BigDecimal(2.0))
    )
  )

  val conf = AgsParams.GmosLongSlit(
    GmosSouthFpu.LongSlit_0_50.asRight,
    PortDisposition.Side
  )

  val searchRadius = 6.arcseconds
  val query        = QueryByADQL(targetCoords,
                          ShapeExpression.centeredEllipse(searchRadius * 2, searchRadius * 2),
                          none
  )

  test("XML file - count stars in GP221000-483213-dr3.xml"):
    val gaia         =
      GaiaClientMock.fromResource[IO]("GP221000-483213-dr3.xml",
                                      NonEmptyChain.one(CatalogAdapter.Gaia3LiteEsaProxy).some
      )
    val searchRadius = 6.arcseconds
    val query        = QueryByADQL(targetCoords,
                            ShapeExpression.centeredEllipse(searchRadius * 2, searchRadius * 2),
                            None
    )

    gaia
      .query(query)
      .map: stars =>
        assertEquals(stars.length, 52)

  test("Run ags with GP221000-483213-dr3.xml"):
    val gaia =
      GaiaClientMock.fromResource[IO]("GP221000-483213-dr3.xml",
                                      NonEmptyChain.one(CatalogAdapter.Gaia3LiteEsaProxy).some
      )

    gaia
      .queryGuideStars(query)
      .map: gs =>
        val r = Ags
          .agsAnalysis(
            constraints,
            wavelength,
            targetCoords,
            List(targetCoords),
            None,
            anglesToTest,
            Some(acqOffsets),
            Some(sciOffsets),
            conf,
            gs.collect { case Right(t) => t }.map(GuideStarCandidate.siderealTarget.get)
          )
        r.sortUsablePositions.collectFirst:
          case Usable(target = GuideStarCandidate(id = id)) => id
      .assertEquals(6479709205473911296L.some)

  test("Run ags with blindOffset matching baseCoordinates"):
    val gaia =
      GaiaClientMock.fromResource[IO](
        "GP221000-483213-dr3.xml",
        NonEmptyChain.one(CatalogAdapter.Gaia3LiteEsaProxy).some
      )

    // if the blind offset is at science it is the same is if it didn't exist
    gaia
      .queryGuideStars(query)
      .map: gs =>
        val r = Ags
          .agsAnalysis(
            constraints,
            wavelength,
            targetCoords,
            List(targetCoords),
            targetCoords.some,
            anglesToTest,
            Some(acqOffsets),
            Some(sciOffsets),
            conf,
            gs.collect { case Right(t) => t }.map(GuideStarCandidate.siderealTarget.get)
          )
        r.sortUsablePositions.collectFirst:
          case Usable(target = GuideStarCandidate(id = id)) => id
      .assertEquals(6479709205473911296L.some)

  test("Run ags with blindOffset 1 degree away"):
    val gaia =
      GaiaClientMock.fromResource[IO]("GP221000-483213-dr3.xml",
                                      NonEmptyChain.one(CatalogAdapter.Gaia3LiteEsaProxy).some
      )

    // with a very far blind offset AGS fails
    val blindOffset =
      targetCoords.offsetBy(Angle.Angle0, Offset.signedDecimalArcseconds.reverseGet(3600, 3600))

    gaia
      .queryGuideStars(query)
      .map: gs =>
        val r = Ags
          .agsAnalysis(
            constraints,
            wavelength,
            targetCoords,
            List(targetCoords),
            blindOffset,
            anglesToTest,
            Some(acqOffsets),
            Some(sciOffsets),
            conf,
            gs.collect { case Right(t) => t }.map(GuideStarCandidate.siderealTarget.get)
          )
        r.sortUsablePositions
      .assertEquals(Nil)
