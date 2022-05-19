// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.`enum`.CloudExtinction
import lucuma.core.`enum`.ImageQuality
import lucuma.core.`enum`.SkyBackground
import lucuma.core.`enum`.WaterVapor
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbConstraintSet {
  import ArbEnumerated._
  import ArbElevationRange._

  implicit val arbConstraintSet: Arbitrary[ConstraintSet] =
    Arbitrary {
      for {
        iq <- arbitrary[ImageQuality]
        ce <- arbitrary[CloudExtinction]
        sb <- arbitrary[SkyBackground]
        wv <- arbitrary[WaterVapor]
        er <- arbitrary[ElevationRange]
      } yield ConstraintSet(iq, ce, sb, wv, er)
    }

  implicit val cogConstraintSet: Cogen[ConstraintSet] =
    Cogen[(ImageQuality, CloudExtinction, SkyBackground, WaterVapor, ElevationRange)]
      .contramap(c => (c.imageQuality, c.cloudExtinction, c.skyBackground, c.waterVapor, c.elevationRange))
}

object ArbConstraintSet extends ArbConstraintSet
