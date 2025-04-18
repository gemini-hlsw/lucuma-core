// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSetupTime {

  import ArbTimeSpan.given

  given Arbitrary[SetupTime] =
    Arbitrary {
      for {
        f <- arbitrary[TimeSpan]
        r <- arbitrary[TimeSpan]
      } yield SetupTime(f, r)
    }

  given Cogen[SetupTime] =
    Cogen[(TimeSpan, TimeSpan)].contramap { a => (
      a.full,
      a.reacquisition
    )}

}

object ArbSetupTime extends ArbSetupTime
