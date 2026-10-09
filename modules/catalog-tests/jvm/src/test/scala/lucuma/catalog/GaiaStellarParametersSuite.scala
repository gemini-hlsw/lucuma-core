// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.syntax.all.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import fs2.io.readClassLoaderResource
import fs2.text
import lucuma.catalog.votable.ADQLInterpreter
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogSearch
import lucuma.catalog.votable.QueryByADQL
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.syntax.all.*
import munit.CatsEffectSuite

/**
 * The VOTables are responses of the NOIRLab Gaia proxy to the blind offset cone search around
 * 13:42:08.10 +09:28:38.4, and to `stellarParametersByIdQuery` for a few stars there.
 */
class GaiaStellarParametersSuite extends CatsEffectSuite:

  private def parsedTargets(xml: String): IO[List[CatalogTargetResult]] =
    fs2.Stream
      .emit(xml)
      .through(CatalogSearch.siderealTargets[IO](CatalogAdapter.Gaia3EsaProxy))
      .compile
      .toList
      .map: results =>
        val (errors, targets) = results.partitionEither(identity)
        assertEquals(errors, Nil)
        targets

  private val coneSearch: IO[String] =
    readClassLoaderResource[IO]("gaia-blind-offset-gmos1.xml")
      .through(text.utf8.decode)
      .compile
      .string

  test("GSP-Phot parameters are parsed from the cone search"):
    coneSearch
      .flatMap(parsedTargets)
      .map: targets =>
        assert(
          targets.exists(
            _.stellarParameters.exists(_.source === GaiaStellarParametersSource.GspPhot)
          )
        )

  test("an unreadable Teff drops the parameters, not the star"):
    val star     = "Gaia DR3 3725398080716580352"
    // teff_gspphot is the 12th column
    val teffCell = s"(<TD>$star</TD>(?:\\s*<TD>[^<]*</TD>){10}\\s*<TD>)[^<]*(</TD>)"
    coneSearch
      .flatMap: xml =>
        val corrupted = xml.replaceFirst(teffCell, "$1bad$2")
        assertNotEquals(corrupted, xml)
        parsedTargets(corrupted)
      .map: targets =>
        val result = targets.find(_.target.name.value === star)
        assert(result.isDefined)
        assertEquals(result.flatMap(_.stellarParameters), None)

  test("ESP-HS id lookup parses only the stars that have values"):
    readClassLoaderResource[IO]("gaia-blind-offset-esphs.xml")
      .through(text.utf8.decode)
      .through(CatalogSearch.stellarParameters(CatalogAdapter.Gaia3EsaProxy))
      .compile
      .toList
      .map: results =>
        val (errors, params) = results.partitionEither(identity)
        assertEquals(errors, Nil)
        assertEquals(
          params.toMap,
          Map(
            3726010543053126656L -> GaiaStellarParameters(8961.withUnit[Kelvin],
                                                          3.227987,
                                                          GaiaStellarParametersSource.EspHs
            )
          )
        )

  test("the blind offset query requests GSP-Phot fields only where the backend has them"):
    val query       =
      QueryByADQL(Coordinates.Zero,
                  ShapeExpression.centeredEllipse(1.arcminutes, 1.arcminutes),
                  None,
                  Angle.Angle0
      )
    val interpreter = ADQLInterpreter.blindOffsetCandidates
    assert(
      interpreter.buildQueryString(CatalogAdapter.Gaia3EsaProxy, query).contains("teff_gspphot")
    )
    assert(
      !interpreter.buildQueryString(CatalogAdapter.Gaia3LiteGavo, query).contains("teff_gspphot")
    )
    assertEquals(CatalogAdapter.Gaia3LiteGavo.stellarParametersByIdQuery(List(1L)), None)
