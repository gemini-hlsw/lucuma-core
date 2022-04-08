// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts

import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.PrecisionModel

/**
 * Shared JTS setup.
 */
object Jts {

  val precisionModel: PrecisionModel =
    new PrecisionModel()

  val geometryFactory: GeometryFactory =
    new GeometryFactory(precisionModel)

  /**
    * Convert jts coordinates back to offset
    */
  def coord2offset(x: Double, y: Double): Offset =
    // Reverse p
    Offset(Offset.P(Angle.fromMicroarcseconds(-x.toLong)),
            Offset.Q(Angle.fromMicroarcseconds(y.toLong))
    )

  def boundingOffsets(g: Geometry): (Offset, Offset) = {
    val envelope    = g.getEnvelopeInternal
    val leftTop     = coord2offset(envelope.getMinX, envelope.getMaxY)
    val bottomRight = coord2offset(envelope.getMaxX, envelope.getMinY)
    (leftTop, bottomRight)
  }

}
