// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package scorpio
package arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbScorpioNodAndShift:

  import ArbOffset.given

  given Arbitrary[ScorpioNodAndShift] =
    Arbitrary:
      for
        a <- arbitrary[Offset]
        b <- arbitrary[Offset]
        o <- arbitrary[PosInt]
        c <- arbitrary[PosInt]
      yield ScorpioNodAndShift(a, b, o, c)

  given Cogen[ScorpioNodAndShift] =
    Cogen[(Offset, Offset, Int, Int)].contramap: a =>
      (a.posA, a.posB, a.shiftOffset.value, a.shiftCycles.value)

object ArbScorpioNodAndShift extends ArbScorpioNodAndShift
