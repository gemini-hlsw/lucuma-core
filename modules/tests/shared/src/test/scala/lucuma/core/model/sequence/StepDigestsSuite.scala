// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbStepDigest.given
import lucuma.core.model.sequence.arb.ArbStepDigests.given
import monocle.law.discipline.*
import munit.*

class StepDigestsSuite extends DisciplineSuite:
  checkAll("Eq[StepDigests]",       EqTests[StepDigests].eqv)
  checkAll("Monoid[StepDigests]",   MonoidTests[StepDigests].monoid)
  checkAll("StepDigests.biases",    LensTests(StepDigests.biases))
  checkAll("StepDigests.darks",     LensTests(StepDigests.darks))
  checkAll("StepDigests.arcs",      LensTests(StepDigests.arcs))
  checkAll("StepDigests.flats",     LensTests(StepDigests.flats))
  checkAll("StepDigests.observing", LensTests(StepDigests.observing))
