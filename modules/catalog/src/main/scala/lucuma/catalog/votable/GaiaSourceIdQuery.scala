// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

trait GaiaSourceIdQuery:

  def idQueryString(id: Long)(using gaia: CatalogAdapter.Gaia): String =
    val fields = gaia.allFields.map(_.id.value.toLowerCase).mkString(",")
    f"""|SELECT $fields
      |     FROM ${gaia.gaiaDB}
      |     WHERE source_id = $id
    """.stripMargin

object GaiaSourceIdQuery extends GaiaSourceIdQuery
