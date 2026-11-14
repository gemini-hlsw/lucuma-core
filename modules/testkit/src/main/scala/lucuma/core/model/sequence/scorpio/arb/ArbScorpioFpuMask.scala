// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package scorpio
package arb

import lucuma.core.enums.ScorpioFpu
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbScorpioFpuMask:

  import ArbEnumerated.given

  given Arbitrary[ScorpioFpuMask] =
    Arbitrary:
      Gen.oneOf(
        Gen.const(ScorpioFpuMask.Imaging),
        arbitrary[ScorpioFpu].map(ScorpioFpuMask.LongSlit(_)),
        Gen.const(ScorpioFpuMask.ColdBlock)
      )

  given Cogen[ScorpioFpuMask] =
    Cogen[(Int, Option[ScorpioFpu])].contramap: a =>
      (a.ordinal, a.longSlit)

object ArbScorpioFpuMask extends ArbScorpioFpuMask
