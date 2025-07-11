// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.ObserveClass
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
        a <- arbitrary[Boolean]
        f <- arbitrary[Boolean]
      yield AtomDigest(i, c, t, a, f)

  given Cogen[AtomDigest] =
    Cogen[(
      Atom.Id,
      ObserveClass,
      CategorizedTime,
      Boolean,
      Boolean
    )].contramap: a =>
      (
        a.id,
        a.observeClass,
        a.timeEstimate,
        a.hasArc,
        a.hasFlat
      )

object ArbAtomDigest extends ArbAtomDigest