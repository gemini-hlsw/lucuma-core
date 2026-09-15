// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbExecutionDigest.given
import lucuma.core.model.sequence.arb.ArbSequenceDigest.given
import lucuma.core.model.sequence.arb.ArbSetupTime.given
import monocle.law.discipline.*
import munit.*

final class ExecutionDigestSuite extends DisciplineSuite:

  checkAll("Eq[ExecutionDigest]",              EqTests[ExecutionDigest].eqv)
  checkAll("ExecutionDigest.setup",            LensTests(ExecutionDigest.setup))
  checkAll("ExecutionDigest.setupCount",       LensTests(ExecutionDigest.setupCount))
  checkAll("ExecutionDigest.calibrationCount", LensTests(ExecutionDigest.calibrationCount))
  checkAll("ExecutionDigest.acquisition",      LensTests(ExecutionDigest.acquisition))
  checkAll("ExecutionDigest.science",          LensTests(ExecutionDigest.science))
