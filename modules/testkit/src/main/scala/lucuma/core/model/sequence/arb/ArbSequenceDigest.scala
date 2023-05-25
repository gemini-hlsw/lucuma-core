// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.arb.ArbBoundedCollection
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

import scala.collection.immutable.SortedSet

trait ArbSequenceDigest {
  import ArbEnumerated.*
  import ArbPlannedTime.given
  import ArbOffset.*

  given Arbitrary[SequenceDigest] =
    Arbitrary {
      for {
        c  <- arbitrary[ObserveClass]
        t  <- arbitrary[PlannedTime]
        o  <- arbitrary[SortedSet[Offset]]
      } yield SequenceDigest(c, t, o)
    }

  given Cogen[SequenceDigest] =
    Cogen[(
      ObserveClass,
      PlannedTime,
      Set[Offset]
    )].contramap { a => (
      a.observeClass,
      a.plannedTime,
      a.offsets
    )}

}

object ArbSequenceDigest extends ArbSequenceDigest
