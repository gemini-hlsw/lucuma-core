// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import lucuma.core.geom.syntax.shapeexpression.*
import lucuma.core.math.Offset

/**
 * One polygon of an evaluated `Shape`, as plain vertices: the exterior ring followed by any
 * holes. Rings are closed (first vertex repeated last).
 */
final case class ShapePolygon(
  exterior: NonEmptyList[Offset],
  holes:    List[NonEmptyList[Offset]]
) derives Eq:

  /** Rebuilds an expression whose evaluation, with any engine, draws this polygon. */
  def toShapeExpression: ShapeExpression =
    holes.foldLeft[ShapeExpression](ShapeExpression.Polygon(exterior.toList))((acc, hole) =>
      acc - ShapeExpression.Polygon(hole.toList)
    )

object ShapePolygon:
  /** A single expression drawing all `polygons`, `Empty` when there are none. */
  def toShapeExpression(polygons: List[ShapePolygon]): ShapeExpression =
    polygons.map(_.toShapeExpression) match
      case Nil          => ShapeExpression.Empty
      case head :: tail => tail.foldLeft(head)(_ ∪ _)
