// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.Order.catsKernelOrderingForOrder
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

import scala.collection.immutable.SortedSet

trait ArbSequenceDigest {
  import ArbEnumerated.given
  import ArbCategorizedTime.given
  import ArbOffset.*

  given Arbitrary[SequenceDigest] =
    Arbitrary {
      for {
        c  <- arbitrary[ObserveClass]
        t  <- arbitrary[CategorizedTime]
        o  <- arbitrary[SortedSet[Offset]]
        n  <- arbitrary[NonNegInt]
      } yield SequenceDigest(c, t, o, n)
    }

  given Cogen[SequenceDigest] =
    Cogen[(
      ObserveClass,
      CategorizedTime,
      Set[Offset],
      NonNegInt
    )].contramap { a => (
      a.observeClass,
      a.timeEstimate,
      a.offsets,
      a.atomCount
    )}

}

object ArbSequenceDigest extends ArbSequenceDigest
