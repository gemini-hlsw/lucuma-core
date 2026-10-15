// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSetupTimeBreakdown:
  import ArbCategorizedTime.given

  given Arbitrary[SetupTimeBreakdown] =
    Arbitrary:
      for
        s <- arbitrary[CategorizedTime]
        a <- arbitrary[CategorizedTime]
        o <- arbitrary[CategorizedTime]
      yield SetupTimeBreakdown(s, a, o)

  given Cogen[SetupTimeBreakdown] =
    Cogen[(CategorizedTime, CategorizedTime, CategorizedTime)].contramap(a => (a.slew, a.acquisition, a.other))

object ArbSetupTimeBreakdown extends ArbSetupTimeBreakdown
