// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbStepDigest.given
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
import munit.*

class StepDigestSuite extends DisciplineSuite:
  checkAll("Eq[StepDigest]",     EqTests[StepDigest].eqv)
  checkAll("Monoid[StepDigest]", MonoidTests[StepDigest].monoid)
