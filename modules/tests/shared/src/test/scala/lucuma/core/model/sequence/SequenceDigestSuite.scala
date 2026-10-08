// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
import lucuma.core.model.sequence.arb.ArbSequenceDigest.given
import lucuma.core.model.sequence.arb.ArbStepDigests.given
import lucuma.core.model.sequence.arb.ArbTelescopeConfig.given
import lucuma.core.model.sequence.exposure.ExposureTimeViolation
import lucuma.core.model.sequence.exposure.arb.ArbExposureTimeViolation.given
import lucuma.core.util.arb.ArbEnumerated.given
import monocle.law.discipline.*
import munit.*

class SequenceDigestSuite extends DisciplineSuite:
  checkAll("Eq[SequenceDigest]",                    EqTests[SequenceDigest].eqv)
  checkAll("Order[ExposureTimeViolation]",          OrderTests[ExposureTimeViolation].order)
  checkAll("SequenceDigest.observeClass",           LensTests(SequenceDigest.observeClass))
  checkAll("SequenceDigest.plannedTime",            LensTests(SequenceDigest.timeEstimate))
  checkAll("SequenceDigest.atomCount",              LensTests(SequenceDigest.atomCount))
  checkAll("SequenceDigest.gcalSets",               LensTests(SequenceDigest.gcalSets))
  checkAll("SequenceDigest.executionState",         LensTests(SequenceDigest.executionState))
  checkAll("SequenceDigest.configs",                LensTests(SequenceDigest.configs))
  checkAll("SequenceDigest.steps",                  LensTests(SequenceDigest.steps))
  checkAll("SequenceDigest.exposureTimeViolations", LensTests(SequenceDigest.exposureTimeViolations))
