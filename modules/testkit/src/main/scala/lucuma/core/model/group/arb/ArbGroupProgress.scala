// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group
package arb

import cats.data.NonEmptyList
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
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
  import ArbGroupConstraintViolation.given
  import ArbGroupConstraintWaiver.given
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

  private val genViolations: Gen[NonEmptyList[GroupConstraintViolation]] =
    for
      v  <- arbitrary[GroupConstraintViolation]
      vs <- Gen.listOfN(2, arbitrary[GroupConstraintViolation])
    yield NonEmptyList(v, vs)

  private val genWaived: Gen[List[GroupConstraintWaiver]] =
    Gen.listOfN(2, arbitrary[GroupConstraintWaiver]).flatMap(Gen.someOf(_)).map(_.toList)

  given Arbitrary[GroupProgress] =
    Arbitrary:
      for
        c  <- arbitrary[NonNegInt]
        r  <- arbitrary[PosInt]
        w  <- genNextWindow
        vs <- genViolations
        ws <- genWaived
        p  <- Gen.oneOf(Empty, Pending(r), Ongoing(c, r, w, ws), Complete(c, r, ws), Unschedulable(c, r, vs, ws))
      yield p

  given Cogen[GroupProgress] =
    Cogen[(
      GroupStatus,
      Int,
      Int,
      Option[TimestampInterval],
      List[GroupConstraintViolation],
      List[GroupConstraintWaiver]
    )].contramap:
      case Empty                       => (GroupStatus.Empty, 0, 0, None, Nil, Nil)
      case Pending(r)                  => (GroupStatus.Pending, 0, r.value, None, Nil, Nil)
      case Ongoing(c, r, w, ws)        => (GroupStatus.Ongoing, c.value, r.value, w, Nil, ws)
      case Complete(c, r, ws)          => (GroupStatus.Complete, c.value, r.value, None, Nil, ws)
      case Unschedulable(c, r, vs, ws) => (GroupStatus.Unschedulable, c.value, r.value, None, vs.toList, ws)

object ArbGroupProgress extends ArbGroupProgress
