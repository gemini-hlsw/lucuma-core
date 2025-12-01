// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.Order.catsKernelOrderingForOrder
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

  given Arbitrary[SequenceDigest] =
    Arbitrary:
      for
        c <- arbitrary[ObserveClass]
        t <- arbitrary[CategorizedTime]
        o <- arbitrary[SortedSet[TelescopeConfig]]
        n <- arbitrary[NonNegInt]
        s <- arbitrary[ExecutionState]
      yield SequenceDigest(c, t, o, n, s)

  given Cogen[SequenceDigest] =
    Cogen[(
      ObserveClass,
      CategorizedTime,
      Set[TelescopeConfig],
      NonNegInt,
      ExecutionState
    )].contramap: a =>
      (
        a.observeClass,
        a.timeEstimate,
        a.configs,
        a.atomCount,
        a.executionState
      )

object ArbSequenceDigest extends ArbSequenceDigest
