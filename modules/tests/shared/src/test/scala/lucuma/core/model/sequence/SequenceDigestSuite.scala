// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbAtom.given
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
import lucuma.core.model.sequence.arb.ArbGcalDigest.given
import lucuma.core.model.sequence.arb.ArbSequenceDigest.given
import lucuma.core.model.sequence.arb.ArbTelescopeConfig.given
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

  property("preserves ordering of atom steps"):
    forAll: (a: Atom[Unit]) =>
      val sd = SequenceDigest.Zero.add(a)
      val result = a.steps.toList.map(s => TelescopeConfig(s.telescopeConfig.offset, s.telescopeConfig.guiding))
      sd.telescopeConfigs === SortedSet.from(result)

  property("counts arc and flat steps"):
    forAll: (a: Atom[Unit]) =>
      val sd    = SequenceDigest.Zero.add(a)
      val steps = a.steps.toList
      val arcs  = steps.filter(_.stepConfig.isArc)
      val flats = steps.filter(_.stepConfig.isFlat)
      (sd.arcs.count.value === arcs.size) &&
      (sd.flats.count.value === flats.size) &&
      (sd.arcs.time === arcs.foldMap(_.timeEstimate)) &&
      (sd.flats.time === flats.foldMap(_.timeEstimate))

  property("observing time excludes arcs and flats"):
    forAll: (a: Atom[Unit]) =>
      val sd    = SequenceDigest.Zero.add(a)
      val other = a.steps.toList.filterNot(s => s.stepConfig.isArc || s.stepConfig.isFlat)
      (sd.observingTime === other.foldMap(_.timeEstimate)) &&
      ((sd.observingTime |+| sd.arcs.time |+| sd.flats.time) === sd.timeEstimate)
