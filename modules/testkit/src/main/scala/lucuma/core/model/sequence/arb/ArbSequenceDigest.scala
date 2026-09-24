// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.arb.ArbTelescopeConfig.given
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

import scala.collection.immutable.SortedSet

trait ArbSequenceDigest:
  import ArbCategorizedTime.given
  import ArbEnumerated.given
  import ArbStepDigest.given

  given Arbitrary[SequenceDigest] =
    Arbitrary:
      for
        c <- arbitrary[ObserveClass]
        t <- arbitrary[CategorizedTime]
        o <- arbitrary[SortedSet[TelescopeConfig]]
        n <- arbitrary[NonNegInt]
        b <- arbitrary[StepDigest]
        d <- arbitrary[StepDigest]
        r <- arbitrary[StepDigest]
        f <- arbitrary[StepDigest]
        v <- arbitrary[StepDigest]
        s <- arbitrary[ExecutionState]
      yield SequenceDigest(c, t, o, n, b, d, r, f, v, s)

  given Cogen[SequenceDigest] =
    Cogen[(
      ObserveClass,
      CategorizedTime,
      Set[TelescopeConfig],
      NonNegInt,
      StepDigest,
      StepDigest,
      StepDigest,
      StepDigest,
      StepDigest,
      ExecutionState
    )].contramap: a =>
      (
        a.observeClass,
        a.timeEstimate,
        a.telescopeConfigs,
        a.atomCount,
        a.biases,
        a.darks,
        a.arcs,
        a.flats,
        a.observing,
        a.executionState
      )

object ArbSequenceDigest extends ArbSequenceDigest
