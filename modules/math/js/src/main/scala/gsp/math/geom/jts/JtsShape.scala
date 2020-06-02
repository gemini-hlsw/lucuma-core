// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom

import gsp.math.Offset
import gsp.math.geom.jts.syntax.offset._
import org.locationtech.jts.geom.Geometry


/**
 * JS / JTS implementation of Shape.
 */
final case class JtsShape(g: Geometry) extends Shape {

  override def contains(o: Offset): Boolean =
    g.contains(o.point)

  override def area: Area =
    Area.fromMicroarcsecondsSquared.getOption(g.getArea.round).getOrElse(Area.MinArea)

  /**
    * Converts to the AWT equivalent at 1 arcsec / pixel.  This is specific to
    * the Java implementation and intended to be used to aid debugging.
    */
   // def toAwt(arcsecPerPixel: Double): java.awt.Shape =
   //   JtsShape.awtWriter(arcsecPerPixel).toShape(g)
}

object JtsShape {
  //
  // import java.awt.geom.Point2D
  // import org.locationtech.jts.awt.{PointTransformation, ShapeWriter}
  // import org.locationtech.jts.geom.Coordinate
  //
  // // Converts points to AWT at arcsec / pixel scale.
  //  private def toArcsec(arcsecPerPixel: Double): PointTransformation =
  //    (s: Coordinate, d: Point2D) => {
  //      val scale = arcsecPerPixel / 1000000.0
  //      d.setLocation(s.x * scale, -s.y * scale)
  //    }
  //
  //  private def awtWriter(arcsecPerPixel: Double): ShapeWriter =
  //    new ShapeWriter(toArcsec(arcsecPerPixel))
  //
}

object JtsApp {
  def main(args: Array[String]): Unit = {
    ()
  }
}
