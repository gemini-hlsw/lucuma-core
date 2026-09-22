// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom
package jts

import lucuma.core.geom.jts.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.util.AffineTransformation

/**
 * JTS implementation of Shape.
 */
final case class JtsShape(g: Geometry) extends Shape {

  def boundingOffsets: BoundingOffsets = {
    val (tl, br) = Jts.boundingOffsets(g)
    BoundingOffsets(tl, br)
  }

  override def contains(o: Offset): Boolean =
    g.contains(o.point)

  /** Angular distance from the origin to the most distant vertex. */
  def radius: Angle =
    val cs = g.getCoordinates
    if   cs.isEmpty then Angle.Angle0
    else cs.maxBy(c => c.x * c.x + c.y * c.y).offset.distance(Offset.Zero)

  override def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(g.getArea.round).getOrElse(Area.MinArea)

  def isEmpty: Boolean = g.isEmpty

  def intersects(that: Shape): Boolean = that match
    case JtsShape(thatG) => g.intersects(thatG)
    case _               => throw new UnsupportedOperationException("Cannot intersect non-JTS shapes")

  def intersection(that: Shape): Shape = that match
    case JtsShape(thatG) => JtsShape(g.intersection(thatG))
    case _               => throw new UnsupportedOperationException("Cannot intersect non-JTS shapes")

  // Same steps as the interpreter; identity steps skipped so the result stays bit-identical.
  def transform(preTranslation: Offset, rotation: Angle, postTranslation: Offset): Shape =
    def translate(geom: Geometry, o: Offset): Geometry =
      val c = o.coordinate
      if (c.x == 0.0 && c.y == 0.0) geom
      else AffineTransformation.translationInstance(c.x, c.y).transform(geom)

    def rotate(geom: Geometry): Geometry =
      if (rotation.toMicroarcseconds == 0L) geom
      else AffineTransformation.rotationInstance(rotation.toDoubleRadians).transform(geom)

    JtsShape(translate(rotate(translate(g, preTranslation)), postTranslation))
}
