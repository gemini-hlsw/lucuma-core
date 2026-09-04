// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbSchedulingAvailability:

  given Arbitrary[SchedulingAvailability] =
    Arbitrary:
      for
        o <- arbitrary[TimeSpan]
        r <- arbitrary[TimeSpan]
      yield SchedulingAvailability(o, r)

  given Cogen[SchedulingAvailability] =
    Cogen[(TimeSpan, TimeSpan)].contramap(w => (w.timeOpen, w.timeRemainingWhenDeclared))

object ArbSchedulingAvailability extends ArbSchedulingAvailability
