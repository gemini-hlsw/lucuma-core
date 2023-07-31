// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen


trait ArbPlannedTimeRange {

  import ArbPlannedTime.given

  given Arbitrary[PlannedTimeRange] =
    Arbitrary {
      for {
        p0 <- arbitrary[PlannedTime]
        p1 <- arbitrary[PlannedTime]
      } yield PlannedTimeRange.from(p0, p1)
    }

  given Cogen[PlannedTimeRange] =
    Cogen[(PlannedTime, PlannedTime)].contramap(a => (a.min, a.max))

}

object ArbPlannedTimeRange extends ArbPlannedTimeRange
