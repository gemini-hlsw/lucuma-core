// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSetupCharge:
  import ArbSetupChargeSummary.given

  given Arbitrary[SetupCharge] =
    Arbitrary:
      for
        f <- arbitrary[SetupChargeSummary]
        r <- arbitrary[SetupChargeSummary]
      yield SetupCharge(f, r)

  given Cogen[SetupCharge] =
    Cogen[(SetupChargeSummary, SetupChargeSummary)].contramap(a => (a.full, a.reacquisition))

object ArbSetupCharge extends ArbSetupCharge
