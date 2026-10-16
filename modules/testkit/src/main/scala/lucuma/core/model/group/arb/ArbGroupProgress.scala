// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group
package arb

import cats.data.NonEmptySet
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GroupConstraintViolation
import lucuma.core.enums.GroupStatus
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimestamp
import lucuma.core.util.arb.ArbTimestampInterval
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import GroupProgress.*

trait ArbGroupProgress:
  import ArbEnumerated.given
  import ArbTimestamp.given
  import ArbTimestampInterval.given

  // Windows with no end of their own as well as bounded ones.
  private val genNextWindow: Gen[Option[TimestampInterval]] =
    Gen.option(
      Gen.oneOf(
        arbitrary[Timestamp].map(TimestampInterval.from),
        arbitrary[TimestampInterval]
      )
    )

  private val genViolations: Gen[NonEmptySet[GroupConstraintViolation]] =
    for
      v  <- arbitrary[GroupConstraintViolation]
      vs <- Gen.listOf(arbitrary[GroupConstraintViolation])
    yield NonEmptySet.of(v, vs*)

  given Arbitrary[GroupProgress] =
    Arbitrary:
      for
        c <- arbitrary[NonNegInt]
        r <- arbitrary[PosInt]
        w <- genNextWindow
        v <- genViolations
        p <- Gen.oneOf(Empty, Pending(r), Ongoing(c, r, w), Complete(c, r), Unschedulable(c, r, v))
      yield p

  given Cogen[GroupProgress] =
    Cogen[(
      GroupStatus,
      Int,
      Int,
      Option[TimestampInterval],
      List[GroupConstraintViolation]
    )].contramap:
      case Empty                  => (GroupStatus.Empty, 0, 0, None, Nil)
      case Pending(r)             => (GroupStatus.Pending, 0, r.value, None, Nil)
      case Ongoing(c, r, w)       => (GroupStatus.Ongoing, c.value, r.value, w, Nil)
      case Complete(c, r)         => (GroupStatus.Complete, c.value, r.value, None, Nil)
      case Unschedulable(c, r, v) => (GroupStatus.Unschedulable, c.value, r.value, None, v.toSortedSet.toList)

object ArbGroupProgress extends ArbGroupProgress
