// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom
package jts

import lucuma.core.geom.jts.syntax.offset._
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

  override def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(g.getArea.round).getOrElse(Area.MinArea)

}
