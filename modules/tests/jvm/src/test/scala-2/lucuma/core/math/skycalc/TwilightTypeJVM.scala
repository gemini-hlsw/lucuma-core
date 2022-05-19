// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import lucuma.core.`enum`.TwilightType

// For use from Java when testing SkyCalc (enum is a reserved keyword)
case class TwilightTypeJVM(wrapped: TwilightType) {
  val horizonAngle = wrapped.horizonAngle.toAngle.toSignedDoubleDegrees
}

object TwilightTypeJVM {
  val OFFICIAL = TwilightType.Official
}
