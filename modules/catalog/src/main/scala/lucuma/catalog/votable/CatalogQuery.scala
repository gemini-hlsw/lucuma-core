// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.*
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.SiderealTracking
import org.http4s.Uri
import spire.math.Bounded

import java.time.Instant

/**
 * Represents a query on a catalog
 */
sealed trait CatalogQuery {

  /**
   * Name of the catalog for this query
   */
  def catalog: CatalogName

  /**
   * Set if a proxy (e.g. cors proxy) in needed
   */
  def proxy: Option[Uri]
}

/**
 * Name based query, e.g. Simbad
 */
case class QueryByName(id: NonEmptyString, proxy: Option[Uri] = None) extends CatalogQuery {
  override val catalog = CatalogName.Simbad
}

trait GaiaBrightnessADQL extends CatalogQuery {
  override val catalog = CatalogName.Gaia

  def circleQuery(base: Coordinates, r: Angle): String =
    f"CIRCLE('ICRS', ${base.ra.toAngle.toDoubleDegrees}%7.5f, ${base.dec.toAngle.toSignedDoubleDegrees}%7.5f, ${r.toDoubleDegrees}%7.5f)"

  def adqlBrightness(brightnessConstraints: Option[BrightnessConstraints]): List[String] =
    brightnessConstraints.foldMap {
      case BrightnessConstraints(bands, faintness, None)             =>
        bands.bands
          .collect {
            case Band.Gaia   => CatalogAdapter.Gaia.gMagField.id
            case Band.GaiaBP => CatalogAdapter.Gaia.bpMagField.id
            case Band.GaiaRP => CatalogAdapter.Gaia.rpMagField.id
          }
          .map(bid => f"($bid < ${faintness.brightness.value.value.toDouble}%.3f)")
      case BrightnessConstraints(bands, faintness, Some(saturation)) =>
        bands.bands
          .collect {
            case Band.Gaia   => CatalogAdapter.Gaia.gMagField.id
            case Band.GaiaBP => CatalogAdapter.Gaia.bpMagField.id
            case Band.GaiaRP => CatalogAdapter.Gaia.rpMagField.id
          }
          .map(bid =>
            f"($bid between ${saturation.brightness.value.value.toDouble}%.3f and ${faintness.brightness.value.value.toDouble}%.3f)"
          )
    }
}

/**
 * Query based on ADQL with a given geometry around base coordinates
 */
sealed trait ADQLQuery {
  def base: Coordinates
  def adqlGeom(using ADQLInterpreter): String
  def adqlBrightness: List[String]
  def proxy: Option[Uri]
}

/**
 * Query based on ADQL with a given geometry around base coordinates
 */
case class QueryByADQL(
  base:                  Coordinates,
  shapeConstraint:       ShapeExpression,
  brightnessConstraints: Option[BrightnessConstraints],
  proxy:                 Option[Uri] = None
) extends CatalogQuery
    with ADQLQuery
    with GaiaBrightnessADQL {
  def adqlBrightness: List[String] = adqlBrightness(brightnessConstraints)

  def adqlGeom(using ev: ADQLInterpreter): String = {
    implicit val si = ev.shapeInterpreter

    val r = shapeConstraint.maxSide.bisect

    circleQuery(base, r)
  }
}

/**
 * Query based on ADQL with a given geometry and coordinates varying on a time range It will
 * calculate a circle centered in the middle of the targets and covering the area for both ends
 *
 * See: https://github.com/gemini-hlsw/lucuma-catalog/wiki/time-range
 */
case class TimeRangeQueryByADQL(
  tracking:              SiderealTracking,
  timeRange:             Bounded[Instant],
  shapeConstraint:       ShapeExpression,
  brightnessConstraints: Option[BrightnessConstraints],
  proxy:                 Option[Uri] = None
) extends CatalogQuery
    with ADQLQuery
    with GaiaBrightnessADQL {
  val base = tracking.baseCoordinates

  def adqlBrightness: List[String] = adqlBrightness(brightnessConstraints)

  def adqlGeom(using ev: ADQLInterpreter): String = {
    given ShapeInterpreter = ev.shapeInterpreter

    // Coordinates at the start and of the time range
    val start = tracking.at(timeRange.lowerBound.a)
    val end   = tracking.at(timeRange.upperBound.a)

    // Try to set the base in the middle of both time ends
    val (offset, base) = (start, end) match {
      case (Some(start), Some(end)) =>
        // offset between them and base at the middle point
        (start.diff(end).offset, start.interpolate(end, 0.5))
      case (Some(start), None)      =>
        // offset between start and original, and base in the middle
        (start.diff(tracking.baseCoordinates).offset,
         start.interpolate(tracking.baseCoordinates, 0.5)
        )
      case (None, Some(end))        =>
        // offset between original and then, and base in the middle
        (tracking.baseCoordinates.diff(end).offset, tracking.baseCoordinates.interpolate(end, 0.5))
      case _                        =>
        // Original values
        (Offset.Zero, tracking.baseCoordinates)
    }
    val r              = (shapeConstraint ∪ (shapeConstraint ↗ offset)).maxSide.bisect

    circleQuery(base, r)
  }
}

/**
 * Query based on ADQL with a given geometry and two coordinates typically at two time points.
 *
 * It is essentially a TimeRangeQuery with precalculated positions See:
 * https://github.com/gemini-hlsw/lucuma-catalog/wiki/time-range
 */
case class CoordinatesRangeQueryByADQL(
  coords:                NonEmptyList[Coordinates],
  shapeConstraint:       ShapeExpression,
  brightnessConstraints: Option[BrightnessConstraints],
  proxy:                 Option[Uri] = None
) extends CatalogQuery
    with ADQLQuery
    with GaiaBrightnessADQL {
  val base = Coordinates.centerOf(coords)

  def adqlBrightness: List[String] = adqlBrightness(brightnessConstraints)

  def adqlGeom(using ev: ADQLInterpreter): String = {
    given ShapeInterpreter = ev.shapeInterpreter

    val r: ShapeExpression = coords
      .map(_.diff(base).offset)
      .foldLeft(shapeConstraint)((prev, offset) => prev ∪ (shapeConstraint ↗ offset))

    circleQuery(base, r.maxSide.bisect)
  }
}
