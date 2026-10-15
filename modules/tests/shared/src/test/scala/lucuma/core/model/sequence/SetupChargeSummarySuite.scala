// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbSetupChargeSummary.given
import lucuma.core.model.sequence.arb.ArbSetupTimeBreakdown.given
import monocle.law.discipline.*
import munit.*

class SetupChargeSummarySuite extends DisciplineSuite:
  checkAll("Order[SetupChargeSummary]",             OrderTests[SetupChargeSummary].order)
  checkAll("CommutativeMonoid[SetupChargeSummary]", CommutativeMonoidTests[SetupChargeSummary].commutativeMonoid)
  checkAll("SetupChargeSummary.count",              LensTests(SetupChargeSummary.count))
  checkAll("SetupChargeSummary.time",               LensTests(SetupChargeSummary.time))
