// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.kernel.laws.discipline.*
import lucuma.core.enums.GroupConstraintViolationCode
import lucuma.core.model.group.arb.ArbGroupConstraintViolation
import lucuma.core.model.group.arb.ArbGroupConstraintWaiver
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

final class GroupConstraintViolationSuite extends DisciplineSuite:

  import ArbGroupConstraintViolation.given
  import ArbGroupConstraintWaiver.given

  checkAll("Eq[GroupConstraintViolation]", EqTests[GroupConstraintViolation].eqv)
  checkAll("Eq[GroupConstraintViolation.Waivable]", EqTests[GroupConstraintViolation.Waivable].eqv)
  checkAll("Eq[GroupConstraintWaiver]", EqTests[GroupConstraintWaiver].eqv)
  checkAll("Eq[GroupConstraintWaiver.Revocation]", EqTests[GroupConstraintWaiver.Revocation].eqv)

  // The kinds of violation that may be waived.
  private val waivable: Set[GroupConstraintViolationCode] =
    Set(
      GroupConstraintViolationCode.Order,
      GroupConstraintViolationCode.MinimumInterval,
      GroupConstraintViolationCode.MaximumInterval,
      GroupConstraintViolationCode.SameNight,
      GroupConstraintViolationCode.ObservationsPerNightMinimum,
      GroupConstraintViolationCode.ObservationsPerNightMaximum
    )

  property("a violation is waivable exactly when its kind is"):
    forAll: (v: GroupConstraintViolation) =>
      val isWaivable = v match
        case _: GroupConstraintViolation.Waivable   => true
        case _: GroupConstraintViolation.Unwaivable => false
      assertEquals(isWaivable, waivable.contains(v.code))
