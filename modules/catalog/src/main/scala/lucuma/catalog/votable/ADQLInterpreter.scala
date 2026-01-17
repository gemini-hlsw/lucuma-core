// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.all.*
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch

/**
 * ADQL queries are quite open thus multiple ways to construct them are possible. The interpreter
 * will take the params of a geometry based query and build restrictions
 */
trait ADQLInterpreter {
  def MaxCount: Int

  def allFields: CatalogAdapter.Gaia => List[FieldId]

  def extraFields(c: Coordinates): List[String]

  def orderBy: Option[String] = None

  def extraConstraints: List[String] = Nil

  given shapeInterpreter: ShapeInterpreter

  /**
   * Builds a query for gaia taking input from the adapter and the query itself.
   */
  def buildQueryString(adapter: CatalogAdapter.Gaia, cs: ADQLQuery): String = {
    val fields           = allFields(adapter).map(_.id.value.toLowerCase).mkString(",")
    val extraFields      = this.extraFields(cs.base)
    val extraFieldsStr   =
      if (extraFields.isEmpty) "" else extraFields.mkString(",", ",", "")
    val (center, radius) = cs.searchParams(using this)
    val orderByStr       = this.orderBy.getOrElse("")
    val extraConstraints =
      if (this.extraConstraints.isEmpty) ""
      else this.extraConstraints.mkString("and (", " and ", ")")

    adapter.geometryQuery(
      fields = fields,
      extraFields = extraFieldsStr,
      center = center,
      radius = radius,
      brightnessConstraints = cs.brightnessConstraints,
      extraConstraints = extraConstraints,
      orderBy = orderByStr,
      maxCount = MaxCount
    )
  }
}

object ADQLInterpreter {

  // Find the target closest to the base. Useful for debugging
  def baseOnly(using si: ShapeInterpreter): ADQLInterpreter =
    new ADQLInterpreter {
      val MaxCount         = 1
      val shapeInterpreter = si

      val allFields: CatalogAdapter.Gaia => List[FieldId] = _.allFields

      override def extraFields(c: Coordinates) =
        List(
          f"DISTANCE(POINT(${c.ra.toAngle.toDoubleDegrees}%9.8f, ${c.dec.toAngle.toSignedDoubleDegrees}%9.8f), POINT(ra, dec)) AS ang_sep"
        )

      override def orderBy =
        Some("ang_sep DESC")
    }

  // Find one target. Useful for debugging
  def oneTarget(using si: ShapeInterpreter): ADQLInterpreter =
    nTarget(1)

  // Find n targets around the base
  def nTarget(count: Int)(using si: ShapeInterpreter): ADQLInterpreter =
    new ADQLInterpreter {
      val MaxCount                                        = count
      val shapeInterpreter                                = si
      val allFields: CatalogAdapter.Gaia => List[FieldId] = _.allFields
      override def orderBy                                = Some("phot_g_mean_mag")
      override def extraFields(c: Coordinates)            = Nil
      override val extraConstraints: List[String]         = List("ruwe < 1.4")
    }

  // Find n targets around the base
  def pmCorrected(count: Int, epoch: Epoch)(using si: ShapeInterpreter): ADQLInterpreter =
    new ADQLInterpreter {
      val MaxCount         = count
      override def orderBy = Some("phot_g_mean_mag")
      val shapeInterpreter = si

      val allFields: CatalogAdapter.Gaia => List[FieldId] =
        adapter =>
          adapter.allFields.filter:
            case adapter.raField | adapter.decField | adapter.pmDecField | adapter.pmRaField |
                adapter.plxField | adapter.rvField =>
              false
            case f if f === adapter.epochField => false
            case _                             => true

      override val extraConstraints: List[String] = List("ruwe < 1.4")

      def extraFields(c: Coordinates): List[String] = List(
        // Gaia can do pm correction for a given epoch
        // https://www.cosmos.esa.int/web/gaia-users/archive/writing-queries/#epoch_prop_pos
        s"COORD1(EPOCH_PROP_POS(ra, dec, parallax, pmra, pmdec, radial_velocity, ref_epoch, ${epoch.epochYear})) as ra",
        s"COORD2(EPOCH_PROP_POS(ra, dec, parallax, pmra, pmdec, radial_velocity, ref_epoch, ${epoch.epochYear})) as dec",
        s"${epoch.epochYear} as ref_epoch"
      )
    }

  // Find blind offset star candidates within 180 arcseconds with G > 12
  def blindOffsetCandidates(using si: ShapeInterpreter): ADQLInterpreter =
    new ADQLInterpreter {
      val MaxCount         = 1000
      val shapeInterpreter = si

      val allFields: CatalogAdapter.Gaia => List[FieldId] = _.allFields

      override def extraFields(c: Coordinates) =
        val centerRa  = f"${c.ra.toAngle.toDoubleDegrees}%7.5f"
        val centerDec = f"${c.dec.toAngle.toSignedDoubleDegrees}%7.5f"
        List(
          s" q3c_dist(ra, dec, $centerRa, $centerDec) as separation"
        )

      override def orderBy = "separation ASC".some

      override val extraConstraints: List[String] =
        List("phot_g_mean_mag > 12.0",
             "phot_g_mean_mag IS NOT NULL",
             "ruwe < 1.4",
             "astrometric_excess_noise < 1"
        )
    }
}
