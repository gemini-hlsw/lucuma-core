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
  import ArbGcalDigest.given

  given Arbitrary[SequenceDigest] =
    Arbitrary:
      for
        c <- arbitrary[ObserveClass]
        t <- arbitrary[CategorizedTime]
        o <- arbitrary[SortedSet[TelescopeConfig]]
        n <- arbitrary[NonNegInt]
        r <- arbitrary[GcalDigest]
        f <- arbitrary[GcalDigest]
        s <- arbitrary[ExecutionState]
      yield SequenceDigest(c, t, o, n, r, f, s)

  given Cogen[SequenceDigest] =
    Cogen[(
      ObserveClass,
      CategorizedTime,
      Set[TelescopeConfig],
      NonNegInt,
      GcalDigest,
      GcalDigest,
      ExecutionState
    )].contramap: a =>
      (
        a.observeClass,
        a.timeEstimate,
        a.telescopeConfigs,
        a.atomCount,
        a.arcs,
        a.flats,
        a.executionState
      )

object ArbSequenceDigest extends ArbSequenceDigest
