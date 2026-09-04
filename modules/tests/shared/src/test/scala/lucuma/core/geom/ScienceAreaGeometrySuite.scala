// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle

trait ScienceAreaGeometrySuite extends munit.FunSuite:

  protected def sides(shape: ShapeExpression): (Angle, Angle) =
    val b = shape.eval.boundingOffsets
    (b.topLeft.p.toAngle.difference(b.bottomRight.p.toAngle),
     b.topLeft.q.toAngle.difference(b.bottomRight.q.toAngle)
    )

  protected def assertCloseArcsec(actual: Angle, expectedArcsec: Double): Unit =
    assertEqualsDouble(actual.toSignedDoubleDegrees * 3600.0, expectedArcsec, 0.01)
