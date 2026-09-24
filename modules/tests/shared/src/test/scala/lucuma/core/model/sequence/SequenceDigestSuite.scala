// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbAtom.given
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
import lucuma.core.model.sequence.arb.ArbStepDigests.given
import lucuma.core.model.sequence.arb.ArbSequenceDigest.given
import lucuma.core.model.sequence.arb.ArbTelescopeConfig.given
import lucuma.core.enums.StepType
import lucuma.core.util.arb.ArbEnumerated.given
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean

import scala.collection.immutable.SortedSet

class SequenceDigestSuite extends DisciplineSuite:
  checkAll("Eq[SequenceDigest]",            EqTests[SequenceDigest].eqv)
  checkAll("SequenceDigest.observeClass",   LensTests(SequenceDigest.observeClass))
  checkAll("SequenceDigest.plannedTime",    LensTests(SequenceDigest.timeEstimate))
  checkAll("SequenceDigest.atomCount",      LensTests(SequenceDigest.atomCount))
  checkAll("SequenceDigest.executionState", LensTests(SequenceDigest.executionState))
  checkAll("SequenceDigest.configs",        LensTests(SequenceDigest.configs))
  checkAll("SequenceDigest.steps",          LensTests(SequenceDigest.steps))

  property("preserves ordering of atom steps"):
    forAll: (a: Atom[Unit]) =>
      val sd = SequenceDigest.Zero.add(a)
      val result = a.steps.toList.map(s => TelescopeConfig(s.telescopeConfig.offset, s.telescopeConfig.guiding))
      sd.telescopeConfigs === SortedSet.from(result)

  private def matches(sd: StepDigest, steps: List[Step[Unit]]): Boolean =
    (sd.count.value === steps.size) && (sd.time === steps.foldMap(_.timeEstimate))

  property("buckets steps by type"):
    forAll: (a: Atom[Unit]) =>
      val sd     = SequenceDigest.Zero.add(a)
      val steps  = a.steps.toList
      val biases = steps.filter(_.stepConfig.stepType === StepType.Bias)
      val darks  = steps.filter(_.stepConfig.stepType === StepType.Dark)
      val arcs   = steps.filter(_.stepConfig.isArc)
      val flats  = steps.filter(s => s.stepConfig.usesGcalUnit && !s.stepConfig.isArc)
      val other  = steps.filter(_.stepConfig.stepType === StepType.Science)
      matches(sd.steps.biases, biases) &&
      matches(sd.steps.darks, darks) &&
      matches(sd.steps.arcs, arcs) &&
      matches(sd.steps.flats, flats) &&
      matches(sd.steps.observing, other)

  property("buckets sum to the time estimate"):
    forAll: (a: Atom[Unit]) =>
      val sd = SequenceDigest.Zero.add(a)
      sd.steps.time === sd.timeEstimate
