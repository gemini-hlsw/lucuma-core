// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.all.*

sealed trait ADQLGaiaQuery {
  val gaia = CatalogAdapter.Gaia

  /**
   * Builds an adql query for gaia taking input from the adapter and the query itself
   */
  def adql(gaia: CatalogAdapter.Gaia, cs: ADQLQuery)(using ci: ADQLInterpreter): String = {
    //
    val fields           = ci.allFields.map(_.id.value.toLowerCase).mkString(",")
    val extraFields      = ci.extraFields(cs.base)
    val extraFieldsStr   =
      if (extraFields.isEmpty) "" else extraFields.mkString(",", ",", "")
    val shapeAdql        = cs.adqlGeom
    val brightnessFields = cs.adqlBrightness
    val brightnessAdql   =
      if (brightnessFields.isEmpty) "" else brightnessFields.mkString("and (", " or ", ")")
    val orderBy          = ci.orderBy.foldMap(s => s"ORDER BY $s")
    val extraConstraints =
      if (ci.extraConstraints.isEmpty) "" else ci.extraConstraints.mkString("and (", " and ", ")")

    val query =
      f"""|SELECT TOP ${ci.MaxCount} $fields $extraFieldsStr
        |     FROM ${gaia.gaiaDB}
        |     WHERE CONTAINS(POINT('ICRS',${gaia.raField.id},${gaia.decField.id}),$shapeAdql)=1
        |     $brightnessAdql
        |     $extraConstraints
        |     $orderBy
      """.stripMargin
    query
  }

}
object ADQLGaiaQuery extends ADQLGaiaQuery
