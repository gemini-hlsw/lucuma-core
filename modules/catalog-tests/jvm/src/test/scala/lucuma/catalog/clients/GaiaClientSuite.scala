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
          assertEquals(target.name.value, "Gaia DR3 538670232718296576")
          assertEquals(tracking.epoch.some, Epoch.Julian.fromEpochYears(2016.0))
          assertEquals(tracking.parallax.map(_.μas.value.value), 166L.some)
          assertEquals(tracking.radialVelocity.map(_.rv.value.toDouble), -39225.376.some)
          assertEquals(
            Target.properMotionRA.getOption(target),
            ProperMotion.μasyRA(-1434).some
          )
          assertEquals(
            Target.properMotionDec.getOption(target),
            ProperMotion.μasyDec(-1064).some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.Gaia).headOption(target),
            BrightnessValue.unsafeFrom(15.083894).withUnit[VegaMagnitude].toMeasureTagged.some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.GaiaBP).headOption(target),
            BrightnessValue.unsafeFrom(15.587215).withUnit[VegaMagnitude].toMeasureTagged.some
          )
          assertEquals(
            Target.integratedBrightnessIn(Band.GaiaRP).headOption(target),
            BrightnessValue.unsafeFrom(14.250962).withUnit[VegaMagnitude].toMeasureTagged.some
          )
        case Left(e)       =>
          fail(s"queryById failed: ${e.toList.mkString("; ")}")

  test("All adapters produce the same results for Barnard's Star"):
    val dataLabClient =
      GaiaClientMock.fromString[IO](barnardStarDataLab,
                                    NonEmptyChain.one(CatalogAdapter.Gaia3DataLab).some
      )

    val gavoClient =
      GaiaClientMock.fromString[IO](barnardStarGavo,
                                    NonEmptyChain.one(CatalogAdapter.Gaia3LiteGavo).some
      )

    val esaClient =
      GaiaClientMock.fromString[IO](barnardStarEsa,
                                    NonEmptyChain.one(CatalogAdapter.Gaia3LiteEsa).some
      )

    // Barnard's Star on gaia
    val sourceId = 4472832130942575872L

    for {
      dataLabResult <- dataLabClient.queryById(sourceId)
      gavoResult    <- gavoClient.queryById(sourceId)
      esaResult     <- esaClient.queryById(sourceId)
    } yield
      val dataLab = dataLabResult.getOrElse(fail("DataLab query failed")).target
      val gavo    = gavoResult.getOrElse(fail("GAVO query failed")).target
      val esa     = esaResult.getOrElse(fail("ESA query failed")).target

      assertEquals(dataLab.tracking.baseCoordinates, gavo.tracking.baseCoordinates)
      assertEquals(dataLab.tracking.baseCoordinates, esa.tracking.baseCoordinates)
      assertEquals(dataLab.tracking.epoch, gavo.tracking.epoch)
      assertEquals(dataLab.tracking.epoch, esa.tracking.epoch)

      // Parallax some differences on precission
      val plxDataLab = dataLab.tracking.parallax.map(_.μas.value.value.toDouble).getOrElse(0.0)
      val plxGavo    = gavo.tracking.parallax.map(_.μas.value.value.toDouble).getOrElse(0.0)
      val plxEsa     = esa.tracking.parallax.map(_.μas.value.value.toDouble).getOrElse(0.0)
      assertEqualsDouble(plxDataLab, plxGavo, 2.0)
      assertEqualsDouble(plxDataLab, plxEsa, 2.0)

      assertEquals(dataLab.tracking.radialVelocity, gavo.tracking.radialVelocity)
      assertEquals(dataLab.tracking.radialVelocity, esa.tracking.radialVelocity)

      // Proper motion some diferences on precession
      val pmRaDataLab  =
        Target.properMotionRA.getOption(dataLab).map(_.μasy.value.toDouble).getOrElse(0.0)
      val pmRaGavo     = Target.properMotionRA.getOption(gavo).map(_.μasy.value.toDouble).getOrElse(0.0)
      val pmRaEsa      = Target.properMotionRA.getOption(esa).map(_.μasy.value.toDouble).getOrElse(0.0)
      val pmDecDataLab =
        Target.properMotionDec.getOption(dataLab).map(_.μasy.value.toDouble).getOrElse(0.0)
      val pmDecGavo    =
        Target.properMotionDec.getOption(gavo).map(_.μasy.value.toDouble).getOrElse(0.0)
      val pmDecEsa     = Target.properMotionDec.getOption(esa).map(_.μasy.value.toDouble).getOrElse(0.0)
      assertEqualsDouble(pmRaDataLab, pmRaGavo, 2.0)
      assertEqualsDouble(pmRaDataLab, pmRaEsa, 2.0)
      assertEqualsDouble(pmDecDataLab, pmDecGavo, 2.0)
      assertEqualsDouble(pmDecDataLab, pmDecEsa, 2.0)

      // G
      val gMagDataLab = Target
        .integratedBrightnessIn(Band.Gaia)
        .headOption(dataLab)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      val gMagGavo    = Target
        .integratedBrightnessIn(Band.Gaia)
        .headOption(gavo)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      val gMagEsa     = Target
        .integratedBrightnessIn(Band.Gaia)
        .headOption(esa)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      assertEqualsDouble(gMagDataLab, gMagGavo, 0.0001)
      assertEqualsDouble(gMagDataLab, gMagEsa, 0.0001)

      // RP
      val rpMagDataLab = Target
        .integratedBrightnessIn(Band.GaiaRP)
        .headOption(dataLab)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      val rpMagGavo    = Target
        .integratedBrightnessIn(Band.GaiaRP)
        .headOption(gavo)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      val rpMagEsa     = Target
        .integratedBrightnessIn(Band.GaiaRP)
        .headOption(esa)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      assertEqualsDouble(rpMagDataLab, rpMagGavo, 0.0001)
      assertEqualsDouble(rpMagDataLab, rpMagEsa, 0.0001)

      // BP
      val bpMagDataLab = Target
        .integratedBrightnessIn(Band.GaiaBP)
        .headOption(dataLab)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      val bpMagGavo    = Target
        .integratedBrightnessIn(Band.GaiaBP)
        .headOption(gavo)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      val bpMagEsa     = Target
        .integratedBrightnessIn(Band.GaiaBP)
        .headOption(esa)
        .map(_.value.value.value.toDouble)
        .getOrElse(0.0)
      assertEqualsDouble(bpMagDataLab, bpMagGavo, 0.0001)
      assertEqualsDouble(bpMagDataLab, bpMagEsa, 0.0001)
