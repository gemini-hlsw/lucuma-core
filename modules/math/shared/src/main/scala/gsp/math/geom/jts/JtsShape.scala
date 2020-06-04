// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom
package jts

import gsp.math.geom.jts.syntax.offset._
import gsp.math.Offset
import org.locationtech.jts.geom.Geometry


/**
 * JTS implementation of Shape.
 */
final case class JtsShape(g: Geometry) extends Shape {

  override def contains(o: Offset): Boolean =
    g.contains(o.point)

  override def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(g.getArea.round).getOrElse(Area.MinArea)

}