// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import lucuma.catalog.AngularSize
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbAngularSize {
  import ArbAngle.given

  given Arbitrary[AngularSize] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
        b <- arbitrary[Angle]
      } yield AngularSize(Angle.AngleOrder.max(a, b), Angle.AngleOrder.min(a, b))
    }

  given Cogen[AngularSize] =
    Cogen[(Angle, Angle)].contramap(a => (a.majorAxis, a.minorAxis))
}

object ArbAngularSize extends ArbAngularSize
