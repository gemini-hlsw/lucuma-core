// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package scorpio
package arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbScorpioDetector:

  import ArbTimeSpan.given

  given Arbitrary[ScorpioDetector] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
      yield ScorpioDetector(t, c)

  given Cogen[ScorpioDetector] =
    Cogen[(TimeSpan, Int)].contramap: a =>
      (a.exposureTime, a.exposureCount.value)

object ArbScorpioDetector extends ArbScorpioDetector
