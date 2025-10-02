// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*
import lucuma.catalog.clients.GaiaClientMock
import lucuma.catalog.votable.*
import lucuma.core.enums.CatalogName
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.syntax.all.*
import munit.CatsEffectSuite

import scala.xml.Utility

class GaiaClientSuite extends CatsEffectSuite with VoTableSamples:

  given ADQLInterpreter = ADQLInterpreter.nTarget(10)

  val testCoords = Coordinates(
    RightAscension.fromDoubleDegrees(95.98749097569124),
    Declination.fromDoubleDegrees(-52.741666247338124).getOrElse(Declination.Zero)
  )

  test("GaiaClient.query returns CatalogTargetResult"):
    val client       = GaiaClientMock.mockGaiaClient[IO](Utility.trim(gaia).toString)
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
    val client       = GaiaClientMock.mockGaiaClient[IO](
      Utility.trim(voTableAlternative).toString,
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
    val client = GaiaClientMock.mockGaiaClient[IO](Utility.trim(gaia).toString)

    client
      .queryById(5500810326779190016L)
      .map:
        case Right(result) =>
          assertEquals(result.target.name.value, "Gaia DR2 5500810326779190016")
          assertEquals(result.target.catalogInfo.map(_.catalog), CatalogName.Gaia.some)
        case Left(e)       =>
          fail(s"queryById failed: ${e.toList.mkString("; ")}")

  test("GaiaClient.queryByIdGuideStar returns Target.Sidereal for single source"):
    val client = GaiaClientMock.mockGaiaClient[IO](
      Utility.trim(voTableAlternative).toString,
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
