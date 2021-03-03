// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import lucuma.core.optics.laws.discipline._

final class SplitMonoSuite extends munit.DisciplineSuite {

  val ex1: SplitMono[Byte, Int] =
    SplitMono(_.toInt, _.toByte)

  val ex2: SplitMono[Int, Long] =
    SplitMono(_.toLong, _.toInt)

  // Laws
  checkAll("Byte < Int", SplitMonoTests(ex1).splitMono)
  checkAll("Int < Long", SplitMonoTests(ex2).splitMono)
  checkAll("Byte < Int < Long", SplitMonoTests(ex1.andThen(ex2)).splitMono)

  test("modify") {
    assertEquals(ex1.modify(_ + 1)(Byte.MaxValue),  Byte.MinValue)
  }

  test("modifyF") {
    assertEquals(ex1.modifyF(i => Option(i + 1))(Byte.MaxValue),  Some(Byte.MinValue))
  }

}
