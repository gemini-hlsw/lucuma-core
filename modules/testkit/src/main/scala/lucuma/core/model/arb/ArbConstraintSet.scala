// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbConstraintSet {
  import ArbElevationRange.given

  given Arbitrary[ConstraintSet] =
    Arbitrary {
      for {
        iq <- arbitrary[ImageQuality.Preset]
        ce <- arbitrary[CloudExtinction.Preset]
        sb <- arbitrary[SkyBackground]
        wv <- arbitrary[WaterVapor]
        er <- arbitrary[ElevationRange]
      } yield ConstraintSet(iq, ce, sb, wv, er)
    }

  given Cogen[ConstraintSet] =
    Cogen[(ImageQuality.Preset, CloudExtinction.Preset, SkyBackground, WaterVapor, ElevationRange)]
      .contramap(c => (c.imageQuality, c.cloudExtinction, c.skyBackground, c.waterVapor, c.elevationRange))
}

object ArbConstraintSet extends ArbConstraintSet
