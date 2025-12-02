// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.eq.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbAtom.given
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
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
      sd.configs === SortedSet.from(result)
