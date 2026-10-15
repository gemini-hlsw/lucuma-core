// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSetupChargeSummary:
  import ArbSetupTimeBreakdown.given

  given Arbitrary[SetupChargeSummary] =
    Arbitrary:
      for
        c <- arbitrary[NonNegInt]
        t <- arbitrary[SetupTimeBreakdown]
      yield SetupChargeSummary(c, t)

  given Cogen[SetupChargeSummary] =
    Cogen[(Int, SetupTimeBreakdown)].contramap(a => (a.count.value, a.time))

object ArbSetupChargeSummary extends ArbSetupChargeSummary
