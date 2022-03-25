// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom
package jts

import lucuma.core.geom.jts.syntax.offset._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Geometry

/**
 * JTS implementation of Shape.
 */
final case class JtsShape(g: Geometry) extends Shape {

  /**
   * Convert jts coordinates back to offset
   */
  private def coord2offset(x: Double, y: Double): Offset =
    // Reverse p
    Offset(Offset.P(Angle.fromMicroarcseconds(-x.toLong)),
           Offset.Q(Angle.fromMicroarcseconds(y.toLong))
    )

  def boundingOffsets: BoundingOffsets = {
    val envelope = g.getEnvelopeInternal
    val p1       = coord2offset(envelope.getMinX, envelope.getMaxY)
    val p2       = coord2offset(envelope.getMaxX, envelope.getMaxY)
    val p3       = coord2offset(envelope.getMaxX, envelope.getMinY)
    val p4       = coord2offset(envelope.getMinX, envelope.getMinY)

    BoundingOffsets(p1, p2, p3, p4)
  }

  override def contains(o: Offset): Boolean =
    g.contains(o.point)

  override def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(g.getArea.round).getOrElse(Area.MinArea)

}
