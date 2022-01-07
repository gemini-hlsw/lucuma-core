// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.math.Angle
import lucuma.core.model.AngularSize
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbAngularSize {
  import lucuma.core.math.arb.ArbAngle._

  implicit val arbAngularSize: Arbitrary[AngularSize] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
        b <- arbitrary[Angle]
      } yield AngularSize(Angle.AngleOrder.max(a, b), Angle.AngleOrder.min(a, b))
    }

  implicit val cogAngularSize: Cogen[AngularSize] =
    Cogen[(Angle, Angle)].contramap(a => (a.majorAxis, a.minorAxis))
}

object ArbAngularSize extends ArbAngularSize
