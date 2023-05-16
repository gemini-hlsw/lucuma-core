// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.ChargeClass
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbPlannedTime {

  import ArbTimeSpan.given
  import ArbEnumerated._

  given Arbitrary[PlannedTime] =
    Arbitrary {
      arbitrary[Set[(ChargeClass, TimeSpan)]].map(PlannedTime.from)
    }

  given Cogen[PlannedTime] =
    Cogen[List[(ChargeClass, TimeSpan)]].contramap(_.charges)

}

object ArbPlannedTime extends ArbPlannedTime
