// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group
package arb

import lucuma.core.enums.GroupConstraintViolationCode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.arb.ArbObservingNight
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import GroupConstraintViolation.Unwaivable
import GroupConstraintViolation.Unwaivable.*
import GroupConstraintViolation.Waivable
import GroupConstraintViolation.Waivable.*

trait ArbGroupConstraintViolation:
  import ArbEnumerated.given
  import ArbGid.given
  import ArbObservingNight.given

  given Arbitrary[Waivable] =
    Arbitrary:
      for
        e <- arbitrary[Either[Group.Id, Observation.Id]]
        n <- arbitrary[ObservingNight]
        v <- Gen.oneOf(
               Order(e),
               MinimumInterval(e),
               MaximumInterval(e),
               SameNight(n),
               ObservationsPerNightMinimum(n),
               ObservationsPerNightMaximum(n)
             )
      yield v

  given Arbitrary[Unwaivable] =
    Arbitrary:
      arbitrary[Group.Id].flatMap: g =>
        Gen.oneOf(
          SameNightMultipleSites,
          ObservationsPerNightMultipleSites,
          InsufficientElements,
          SubgroupUnschedulable(g)
        )

  given Arbitrary[GroupConstraintViolation] =
    Arbitrary:
      Gen.oneOf(arbitrary[Waivable], arbitrary[Unwaivable])

  given Cogen[GroupConstraintViolation] =
    Cogen[(
      GroupConstraintViolationCode,
      Option[Either[Group.Id, Observation.Id]],
      Option[ObservingNight],
      Option[Group.Id]
    )].contramap: v =>
      v match
        case Order(e)                       => (v.code, Some(e), None, None)
        case MinimumInterval(e)             => (v.code, Some(e), None, None)
        case MaximumInterval(e)             => (v.code, Some(e), None, None)
        case SameNight(n)                   => (v.code, None, Some(n), None)
        case ObservationsPerNightMinimum(n) => (v.code, None, Some(n), None)
        case ObservationsPerNightMaximum(n) => (v.code, None, Some(n), None)
        case SubgroupUnschedulable(g)       => (v.code, None, None, Some(g))
        case _                              => (v.code, None, None, None)

  given Cogen[Waivable] =
    Cogen[GroupConstraintViolation].contramap(identity)

object ArbGroupConstraintViolation extends ArbGroupConstraintViolation
