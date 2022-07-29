// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts.syntax

import lucuma.core.math.Angle

// Syntax used in the JTS implementation only.

final class AngleOps(private val self: Angle) extends AnyVal {
  def µas: Long =
    Angle.signedMicroarcseconds.get(self)
}

trait ToAngleOps {
  implicit def ToAngleOps(a: Angle): AngleOps =
    new AngleOps(a)
}

object angle extends ToAngleOps
