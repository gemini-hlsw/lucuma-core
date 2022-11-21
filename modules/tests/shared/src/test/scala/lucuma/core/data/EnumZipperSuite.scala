// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data

import cats.kernel.laws.discipline.EqTests
import cats.syntax.all._
import lucuma.core.enums.StepType
import lucuma.core.enums.StepType._
import lucuma.core.util.arb.ArbEnumerated._

import arb.ArbEnumZipper._

final class EnumZipperSuite extends munit.DisciplineSuite {
  test("withFocus on focus") {
    // move two positions to the right
    val z1 = EnumZipper.of[StepType].next.flatMap(_.next)
    assert(z1.map(_.withFocus(Gcal)).exists(_.focus === Gcal))
    assert(z1.map(_.withFocus(Gcal)).exists(_.lefts === List(Dark, Bias)))
    assert(z1.map(_.withFocus(Gcal)).exists(_.rights == List(Science, SmartGcal)))
  }
  test("withFocus on left") {
    // move two positions to the right
    val z1 = EnumZipper.of[StepType].next.flatMap(_.next)
    assert(z1.map(_.withFocus(Bias)).exists(_.focus === Bias))
    assert(z1.map(_.withFocus(Bias)).exists(_.lefts.isEmpty))
    assert(z1.map(_.withFocus(Bias)).exists(_.rights == List(Dark, Gcal, Science, SmartGcal)))
    assert(z1.map(_.withFocus(Dark)).exists(_.focus === Dark))
    assert(z1.map(_.withFocus(Dark)).exists(_.lefts === List(Bias)))
    assert(z1.map(_.withFocus(Dark)).exists(_.rights == List(Gcal, Science, SmartGcal)))
  }
  test("withFocus on right") {
    // move two positions to the right
    val z1 = EnumZipper.of[StepType].next.flatMap(_.next)
    assert(z1.map(_.withFocus(Science)).exists(_.focus === Science))
    assert(z1.map(_.withFocus(Science)).exists(_.lefts === List(Gcal, Dark, Bias)))
    assert(z1.map(_.withFocus(Science)).exists(_.rights == List(SmartGcal)))
    assert(z1.map(_.withFocus(SmartGcal)).exists(_.focus === SmartGcal))
    assert(z1.map(_.withFocus(SmartGcal)).exists(_.lefts === List(Science, Gcal, Dark, Bias)))
    assert(z1.map(_.withFocus(SmartGcal)).exists(_.rights.isEmpty))
  }
  checkAll("Eq[EnumZipper]", EqTests[EnumZipper[StepType]].eqv)
}
