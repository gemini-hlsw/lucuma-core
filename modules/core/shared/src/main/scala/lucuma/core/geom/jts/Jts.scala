// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts

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

}
