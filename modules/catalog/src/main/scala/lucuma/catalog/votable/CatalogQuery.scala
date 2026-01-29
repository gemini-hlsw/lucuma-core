// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.*
import lucuma.core.enums.CatalogName
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.SiderealTracking
import lucuma.core.syntax.all.*
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
}

/**
 * Name based query, e.g. Simbad
 */
case class QueryByName(id: NonEmptyString, proxy: Option[Uri] = None) extends CatalogQuery {
  override val catalog = CatalogName.Simbad
}

/**
 * Query based on ADQL with a given geometry around base coordinates
 */
sealed trait ADQLQuery {
  def base: Coordinates
  // Returns center points and radius
  def searchParams(using ADQLInterpreter): (Coordinates, Angle)
  def brightnessConstraints: Option[BrightnessConstraints]
}

object ADQLQuery:
  // Gaia DR3 positions are at epoch 2016.0.
  //
  // To include high-proper-motion stars (e.g. Barnard's star at ~10.4"/yr) that may have been
  // outside the patrol field in 2016 but are now viable candidates, we pad the query radius
  // by ~3 arcmin to reach ~10 arcmin total.
  //
  // We used to call this embiggen on the ocs though it was related to caching.
  val DefaultAreaBuffer: Angle = 183.arcseconds

/**
 * Query based on ADQL with a given geometry around base coordinates
 */
case class QueryByADQL(
  base:                  Coordinates,
  shapeConstraint:       ShapeExpression,
  brightnessConstraints: Option[BrightnessConstraints],
  areaBuffer:            Angle = ADQLQuery.DefaultAreaBuffer
) extends CatalogQuery
    with ADQLQuery {
  override val catalog = CatalogName.Gaia

  def searchParams(using ev: ADQLInterpreter): (Coordinates, Angle) = {
    given ShapeInterpreter = ev.shapeInterpreter

    (base, shapeConstraint.maxSide.bisect + areaBuffer)
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
  proxy:                 Option[Uri] = None,
  areaBuffer:            Angle = ADQLQuery.DefaultAreaBuffer
) extends CatalogQuery
    with ADQLQuery {
  override val catalog = CatalogName.Gaia

  val base = tracking.baseCoordinates

  def searchParams(using ev: ADQLInterpreter): (Coordinates, Angle) = {
    given ShapeInterpreter = ev.shapeInterpreter

    // Coordinates at the start and of the time range
    val start = tracking.at(timeRange.lowerBound.a)
    val end   = tracking.at(timeRange.upperBound.a)

    // Try to set the base in the middle of both time ends
    val (offset, center) = (start, end) match {
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

    (center, (shapeConstraint ∪ (shapeConstraint ↗ offset)).maxSide.bisect + areaBuffer)
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
  proxy:                 Option[Uri] = None,
  areaBuffer:            Angle = ADQLQuery.DefaultAreaBuffer
) extends CatalogQuery
    with ADQLQuery {
  override val catalog = CatalogName.Gaia

  val base = Coordinates.centerOf(coords)

  def searchParams(using ev: ADQLInterpreter): (Coordinates, Angle) = {
    given ShapeInterpreter = ev.shapeInterpreter

    val shape: ShapeExpression = coords
      .map(_.diff(base).offset)
      .foldLeft(shapeConstraint)((prev, offset) => prev ∪ (shapeConstraint ↗ offset))

    (base, shape.maxSide.bisect + areaBuffer)
  }
}
