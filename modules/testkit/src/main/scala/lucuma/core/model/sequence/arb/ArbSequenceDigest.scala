// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.arb.ArbTelescopeConfig.given
import lucuma.core.model.sequence.exposure.ExposureTimeViolation
import lucuma.core.model.sequence.exposure.arb.ArbExposureTimeViolation.given
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import scala.collection.immutable.SortedSet

trait ArbSequenceDigest:
  import ArbCategorizedTime.given
  import ArbEnumerated.given
  import ArbStepDigests.given

  given Arbitrary[SequenceDigest] =
    Arbitrary:
      for
        c <- arbitrary[ObserveClass]
        t <- arbitrary[CategorizedTime]
        o <- arbitrary[SortedSet[TelescopeConfig]]
        n <- arbitrary[NonNegInt]
        g <- arbitrary[NonNegInt]
        d <- arbitrary[StepDigests]
        s <- arbitrary[ExecutionState]
        v <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, arbitrary[ExposureTimeViolation])).map(SortedSet.from)
      yield SequenceDigest(c, t, o, n, g, d, s, v)

  given Cogen[SequenceDigest] =
    Cogen[(
      ObserveClass,
      CategorizedTime,
      Set[TelescopeConfig],
      NonNegInt,
      NonNegInt,
      StepDigests,
      ExecutionState,
      List[ExposureTimeViolation]
    )].contramap: a =>
      (
        a.observeClass,
        a.timeEstimate,
        a.telescopeConfigs,
        a.atomCount,
        a.gcalSets,
        a.steps,
        a.executionState,
        a.exposureTimeViolations.toList
      )

object ArbSequenceDigest extends ArbSequenceDigest
