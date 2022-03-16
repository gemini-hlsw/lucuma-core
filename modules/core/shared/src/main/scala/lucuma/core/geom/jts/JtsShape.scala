// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom
package jts

import lucuma.core.geom.jts.syntax.offset._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Geometry
import scala.math.sqrt

/**
 * JTS implementation of Shape.
 */
final case class JtsShape(g: Geometry) extends Shape {
  def circumscribedRadius: Angle = {
    val envelope = g.getEnvelopeInternal
    val height = envelope.getHeight
    val width = envelope.getWidth
    val radius = sqrt(height*height + width*width)/2
    Angle.fromMicroarcseconds(radius.toLong)
  }

  private def coord2offset(x: Double, y: Double): Offset =
    Offset(Offset.P(Angle.fromMicroarcseconds(x.toLong)), Offset.Q(Angle.fromMicroarcseconds(y.toLong)))

  def boundingBox: (Offset, Offset, Offset, Offset) = {
    val envelope = g.getEnvelopeInternal
    val c1 = coord2offset(-envelope.getMinX, envelope.getMinY)
    val c2 = coord2offset(-envelope.getMaxX, envelope.getMinY)
    val c3 = coord2offset(-envelope.getMaxX, envelope.getMaxY)
    val c4 = coord2offset(-envelope.getMinX, envelope.getMaxY)

    (c1, c2, c3, c4)
  }

  override def contains(o: Offset): Boolean =
    g.contains(o.point)

  override def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(g.getArea.round).getOrElse(Area.MinArea)

}
