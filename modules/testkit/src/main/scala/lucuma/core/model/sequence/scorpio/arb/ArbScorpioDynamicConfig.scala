// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package scorpio
package arb

import lucuma.core.enums.ScorpioDisperser
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbScorpioDynamicConfig:

  import ArbEnumerated.given
  import ArbScorpioChannelConfig.given
  import ArbScorpioFpuMask.given

  given Arbitrary[ScorpioDynamicConfig] =
    Arbitrary:
      for
        g  <- arbitrary[Option[ScorpioChannelConfig]]
        r  <- arbitrary[Option[ScorpioChannelConfig]]
        i  <- arbitrary[Option[ScorpioChannelConfig]]
        z  <- arbitrary[Option[ScorpioChannelConfig]]
        y  <- arbitrary[Option[ScorpioChannelConfig]]
        j  <- arbitrary[Option[ScorpioChannelConfig]]
        h  <- arbitrary[Option[ScorpioChannelConfig]]
        ks <- arbitrary[Option[ScorpioChannelConfig]]
        f  <- arbitrary[ScorpioFpuMask]
        d  <- arbitrary[ScorpioDisperser]
      yield ScorpioDynamicConfig(g, r, i, z, y, j, h, ks, f, d)

  given Cogen[ScorpioDynamicConfig] =
    Cogen[(List[Option[ScorpioChannelConfig]], ScorpioFpuMask, ScorpioDisperser)].contramap: a =>
      (List(a.g, a.r, a.i, a.z, a.y, a.j, a.h, a.ks), a.fpu, a.disperser)

object ArbScorpioDynamicConfig extends ArbScorpioDynamicConfig
