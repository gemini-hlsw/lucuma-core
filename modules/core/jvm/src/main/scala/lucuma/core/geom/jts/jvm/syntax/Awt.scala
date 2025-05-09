// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts
package jvm.syntax

import org.locationtech.jts.awt.ShapeWriter
import org.locationtech.jts.geom.Coordinate

import java.awt.geom.Point2D

trait ToAwtOps {
  extension(s: JtsShape)
    /**
    * Converts to the AWT equivalent at provided scale. This is specific to the JVM implementation
    * and intended to be used to aid debugging.
    */
    def toAwt(arcsecPerPixel: Double): java.awt.Shape = {
      val sw = new ShapeWriter((s: Coordinate, d: Point2D) => {
        val scale = arcsecPerPixel / 1000000.0
        d.setLocation(s.x * scale, -s.y * scale)
      })

      sw.toShape(s.g)
    }
}

object awt extends ToAwtOps
