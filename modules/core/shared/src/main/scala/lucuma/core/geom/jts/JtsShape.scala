// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom
package jts

import lucuma.core.geom.jts.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Geometry

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

}
