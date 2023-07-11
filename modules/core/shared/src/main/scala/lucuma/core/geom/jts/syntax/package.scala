// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts

import lucuma.core.geom.jts.Jts.geometryFactory
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Envelope
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.util.GeometricShapeFactory

object syntax {

  // Syntax used in the JTS implementation only.

  object all {
    extension (self: Angle)
      def µas: Long =
        Angle.signedMicroarcseconds.get(self)

    extension(self: Offset)

      def µas: (Long, Long) =
        (self.p.toAngle.µas, self.q.toAngle.µas)

      def coordinate: Coordinate = {
        val (p, q) = µas
        // Offset p is flipped around the y axis so it increases to the left
        new Coordinate(-p.toDouble, q.toDouble)
      }

      def point: Geometry =
        geometryFactory.createPoint(coordinate)

      def envelope(that: Offset): Envelope =
        new Envelope(coordinate, that.coordinate)

      def shapeFactory(that: Offset): GeometricShapeFactory = {
        val f = new GeometricShapeFactory(geometryFactory)
        f.setEnvelope(envelope(that))
        f
      }
  }
}
