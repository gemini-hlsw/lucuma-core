// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import lucuma.catalog.clients.GaiaClientMock
import lucuma.catalog.votable.*
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.ProperMotion
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.Target
import lucuma.core.syntax.all.*
import munit.CatsEffectSuite

class GaiaClientSuite extends CatsEffectSuite with VoTableSamples:

  given ADQLInterpreter = ADQLInterpreter.nTarget(10)

  val testCoords = Coordinates(
    RightAscension.fromDoubleDegrees(95.98749097569124),
    Declination.fromDoubleDegrees(-52.741666247338124).getOrElse(Declination.Zero)
  )

  test("GaiaClient.query returns CatalogTargetResult"):
    val client       = GaiaClientMock.fromXML[IO](
      gaia,
      NonEmptyChain.of(CatalogAdapter.Gaia3LiteGavo, CatalogAdapter.Gaia3LiteEsaProxy).some
    )
    val searchRadius = 6.arcseconds
    val query        = QueryByADQL(testCoords,
                            ShapeExpression.centeredEllipse(searchRadius * 2, searchRadius * 2),
                            None
    )

    client
      .query(query)
      .map: results =>
        val target = results.headOption.flatMap(_.toOption).map(_.target)
        assert(target.isDefined)
        assertEquals(target.get.name.value, "Gaia DR2 5500810292414804352")
        assertEquals(target.get.catalogInfo.map(_.catalog), CatalogName.Gaia.some)

  test("GaiaClient.queryGuideStars returns minimal Target.Sidereal"):
    val client       = GaiaClientMock.fromXML[IO](voTableAlternative,
                                            NonEmptyChain.one(CatalogAdapter.Gaia3LiteGavo).some
    )
    val searchRadius = 6.arcseconds
    val query        = QueryByADQL(testCoords,
                            ShapeExpression.centeredEllipse(searchRadius * 2, searchRadius * 2),
                            None
    )

    client
      .queryGuideStars(query)
      .map: results =>
        val target = results.headOption.flatMap(_.toOption)
        assert(target.isDefined)
        // Guide star names include "Gaia DR3" prefix from Gaia3Lite.parseName
        assertEquals(target.get.name.value, "Gaia DR3 5717302551387263616")
        assert(target.get.catalogInfo.isEmpty)

  test("GaiaClient.queryById returns CatalogTargetResult for single source"):
    val client = GaiaClientMock.fromXML[IO](
      gaia,
      NonEmptyChain.of(CatalogAdapter.Gaia3LiteGavo, CatalogAdapter.Gaia3LiteEsaProxy).some
    )

    client
      .queryById(5500810326779190016L)
      .map:
        case Right(result) =>
          assertEquals(result.target.name.value, "Gaia DR2 5500810326779190016")
          assertEquals(result.target.catalogInfo.map(_.catalog), CatalogName.Gaia.some)
        case Left(e)       =>
          fail(s"queryById failed: ${e.toList.mkString("; ")}")

  test("GaiaClient.queryByIdGuideStar returns Target.Sidereal for single source"):
    val client = GaiaClientMock.fromXML[IO](voTableAlternative,
                                            NonEmptyChain.one(CatalogAdapter.Gaia3LiteGavo).some
    )

    client
      .queryByIdGuideStar(5717266885975823616L)
      .map:
        case Right(target) =>
          assertEquals(target.name.value, "Gaia DR3 5717266885975823616")
          assertEquals(target.catalogInfo.map(_.catalog), none)
        case Left(e)       =>
          fail(s"queryByIdGuideStar failed: ${e.toList.mkString("; ")}")

  test("Gaia3LiteGavo adapter for px, rv, and brightness"):
    val client = GaiaClientMock.fromString[IO](gavoParallaxAndRV,
                                               NonEmptyChain.one(CatalogAdapter.Gaia3LiteGavo).some
    )

    client
      .queryById(538670232718296576L)
      .map:
        case Right(result) =>
          val tracking = result.target.tracking
          // parallax: 0.16641381 mas -> 166 μas as long
          assertEquals(tracking.parallax.map(_.μas.value.value), 166L.some)
          // radial_velocity: -39.225376 km/s -> -39225.376 m/s
          assertEquals(tracking.radialVelocity.map(_.rv.value.toDouble), -39225.376.some)
          assertEquals(
            Target.integratedBrightnessIn(Band.Gaia).headOption(result.target),
            BrightnessValue.unsafeFrom(15.083894).withUnit[VegaMagnitude].toMeasureTagged.some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.GaiaRP).headOption(result.target),
            BrightnessValue.unsafeFrom(14.250962).withUnit[VegaMagnitude].toMeasureTagged.some
          )
        case Left(e)       =>
          fail(s"queryById failed: ${e.toList.mkString("; ")}")

  test("Gaia3LiteEsa adapter for px, rv, and brightness"):
    val client = GaiaClientMock.fromString[IO](esaLiteParallaxAndRV,
                                               NonEmptyChain.one(CatalogAdapter.Gaia3LiteEsa).some
    )

    client
      .queryById(538670232718296576L)
      .map:
        case Right(result) =>
          val tracking = result.target.tracking
          assertEquals(tracking.parallax.map(_.μas.value.value), 166L.some)
          assertEquals(tracking.radialVelocity.map(_.rv.value.toDouble), -39225.376.some)
          assertEquals(
            Target.integratedBrightnessIn(Band.Gaia).headOption(result.target),
            BrightnessValue.unsafeFrom(15.083894).withUnit[VegaMagnitude].toMeasureTagged.some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.GaiaRP).headOption(result.target),
            BrightnessValue.unsafeFrom(14.250962).withUnit[VegaMagnitude].toMeasureTagged.some
          )
        case Left(e)       =>
          fail(s"queryById failed: ${e.toList.mkString("; ")}")

  test("GaiaClient parses parallax and radial velocity with Gaia3Esa adapter"):
    val client = GaiaClientMock
      .fromString[IO](esaFullParallaxAndRV, NonEmptyChain.one(CatalogAdapter.Gaia3Esa).some)

    client
      .queryById(538670232718296576L)
      .map:
        case Right(result) =>
          val tracking = result.target.tracking
          assertEquals(tracking.parallax.map(_.μas.value.value), 166L.some)
          assertEquals(tracking.radialVelocity.map(_.rv.value.toDouble), -39225.376.some)
        case Left(e)       =>
          fail(s"queryById failed: ${e.toList.mkString("; ")}")

  test("Gaia3DataLab adapter for all fields"):
    val client = GaiaClientMock
      .fromString[IO](dataLabSample, NonEmptyChain.one(CatalogAdapter.Gaia3DataLab).some)

    client
      .queryById(538670232718296576L)
      .map:
        case Right(result) =>
          val target   = result.target
          val tracking = target.tracking
          // name from designation field
          assertEquals(target.name.value, "Gaia DR3 538670232718296576")
          // epoch from ref_epoch field
          assertEquals(tracking.epoch.some, Epoch.Julian.fromEpochYears(2016.0))
          // parallax and radial velocity
          assertEquals(tracking.parallax.map(_.μas.value.value), 166L.some)
          assertEquals(tracking.radialVelocity.map(_.rv.value.toDouble), -39225.376.some)
          // proper motion: pmra=-1.4343 mas/yr, pmdec=-1.0646 mas/yr
          assertEquals(
            Target.properMotionRA.getOption(target),
            ProperMotion.μasyRA(-1434).some
          )
          assertEquals(
            Target.properMotionDec.getOption(target),
            ProperMotion.μasyDec(-1064).some
          )
          // brightness
          assertEquals(
            Target.integratedBrightnessIn(Band.Gaia).headOption(target),
            BrightnessValue.unsafeFrom(15.083894).withUnit[VegaMagnitude].toMeasureTagged.some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.GaiaRP).headOption(target),
            BrightnessValue.unsafeFrom(14.250962).withUnit[VegaMagnitude].toMeasureTagged.some
          )
        case Left(e)       =>
          fail(s"queryById failed: ${e.toList.mkString("; ")}")
