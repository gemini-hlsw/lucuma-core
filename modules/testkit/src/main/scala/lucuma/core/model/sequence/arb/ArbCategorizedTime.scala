// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

trait ArbCategorizedTime {

  import ArbEnumerated.given
  import ArbTimeSpan.given

  given Arbitrary[CategorizedTime] =
    Arbitrary {
      arbitrary[Set[(ChargeClass, TimeSpan)]].map(CategorizedTime.from)
    }

  given Cogen[CategorizedTime] =
    Cogen[List[(ChargeClass, TimeSpan)]].contramap(_.charges)

}

object ArbCategorizedTime extends ArbCategorizedTime
