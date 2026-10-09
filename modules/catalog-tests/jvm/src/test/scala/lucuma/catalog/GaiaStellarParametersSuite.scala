// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.syntax.all.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import fs2.io.readClassLoaderResource
import fs2.text
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogSearch
import munit.CatsEffectSuite

/**
 * The VOTables are responses of the NOIRLab Gaia proxy and of DataLab to
 * `stellarParametersByIdQuery` for the same five stars.
 */
class GaiaStellarParametersSuite extends CatsEffectSuite:

  private val HotStar: Long  = 3726010543053126656L
  private val NoValues: Long = 3266527962404665088L

  private def lookup(
    file:    String,
    adapter: CatalogAdapter
  ): IO[Map[Long, GaiaStellarParameters]] =
    readClassLoaderResource[IO](file)
      .through(text.utf8.decode)
      .through(CatalogSearch.stellarParameters(adapter))
      .compile
      .toList
      .map: results =>
        val (errors, params) = results.partitionEither(identity)
        assertEquals(errors, Nil)
        params.toMap

  private def gspPhot(teff: Int, logG: Double) =
    GaiaStellarParameters(teff.withUnit[Kelvin], logG, GaiaStellarParametersSource.GspPhot)

  test("ESA lookup prefers ESP-HS and falls back to GSP-Phot"):
    lookup("gaia-stellar-parameters-esa.xml", CatalogAdapter.Gaia3EsaProxy).map: params =>
      assertEquals(
        params,
        Map(
          3725398080716580352L -> gspPhot(5597, 4.5355),
          2109057984352250368L -> gspPhot(3785, 4.732),
          628705013665242240L  -> gspPhot(5569, 4.3463),
          HotStar              -> GaiaStellarParameters(8961.withUnit[Kelvin],
                                           3.227987,
                                           GaiaStellarParametersSource.EspHs
          )
        )
      )

  test("DataLab lookup gives GSP-Phot and treats NaN as missing"):
    lookup("gaia-stellar-parameters-datalab.xml", CatalogAdapter.Gaia3DataLab).map: params =>
      assertEquals(params.keySet,
                   Set(3725398080716580352L, 2109057984352250368L, 628705013665242240L)
      )
      assert(params.values.forall(_.source === GaiaStellarParametersSource.GspPhot))
      assertEquals(params.get(3725398080716580352L), Some(gspPhot(5597, 4.5355)))
      assertEquals(params.get(HotStar), None)
      assertEquals(params.get(NoValues), None)

  test("an unreadable Teff drops that star only"):
    readClassLoaderResource[IO]("gaia-stellar-parameters-esa.xml")
      .through(text.utf8.decode)
      .compile
      .string
      .map(_.replace("<TD>5597.1294</TD>", "<TD>bad</TD>"))
      .flatMap: xml =>
        fs2.Stream
          .emit(xml)
          .through(CatalogSearch.stellarParameters[IO](CatalogAdapter.Gaia3EsaProxy))
          .compile
          .toList
      .map: results =>
        val (errors, params) = results.partitionEither(identity)
        assertEquals(errors.length, 1)
        assertEquals(params.toMap.keySet, Set(2109057984352250368L, 628705013665242240L, HotStar))

  test("lookup queries exist for ESA-family adapters and DataLab, not GAVO"):
    val ids = List(1L, 2L)
    assert(
      CatalogAdapter.Gaia3EsaProxy
        .stellarParametersByIdQuery(ids)
        .exists(q => q.contains("gaiadr3.astrophysical_parameters") && q.contains("teff_esphs"))
    )
    assert(CatalogAdapter.Gaia3LiteEsa.stellarParametersByIdQuery(ids).isDefined)
    assert(
      CatalogAdapter.Gaia3DataLab
        .stellarParametersByIdQuery(ids)
        .exists(q => q.contains("teff_gspphot") && !q.contains("esphs"))
    )
    assertEquals(CatalogAdapter.Gaia3LiteGavo.stellarParametersByIdQuery(ids), None)
    assertEquals(CatalogAdapter.Gaia3EsaProxy.stellarParametersByIdQuery(Nil), None)
