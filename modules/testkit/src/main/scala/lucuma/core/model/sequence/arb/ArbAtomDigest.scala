// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepType
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbAtomDigest:

  import ArbEnumerated.given
  import ArbCategorizedTime.given
  import ArbUid.given

  given Arbitrary[AtomDigest] =
    Arbitrary:
      for
        i <- arbitrary[Atom.Id]
        c <- arbitrary[ObserveClass]
        t <- arbitrary[CategorizedTime]
        s <- arbitrary[Set[StepType]]
        l <- arbitrary[Set[GcalLampType]]
        n <- arbitrary[NonNegInt]
        p <- arbitrary[PosInt]
      yield AtomDigest(i, c, t, s, l, n, p)

  given Cogen[AtomDigest] =
    Cogen[(
      Atom.Id,
      ObserveClass,
      CategorizedTime,
      List[StepType],
      List[GcalLampType],
      Int,
      Int
    )].contramap: a =>
      (
        a.id,
        a.observeClass,
        a.timeEstimate,
        a.stepTypes.toList,
        a.lampTypes.toList,
        a.stepIndex.value,
        a.stepCount.value
      )

object ArbAtomDigest extends ArbAtomDigest