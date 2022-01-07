// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import lucuma.core.optics.laws.discipline._

final class SplitEpiSuite extends munit.DisciplineSuite {

  val ex1: SplitEpi[Long, Int] =
    SplitEpi(_.toInt, _.toLong)

  val ex2: SplitEpi[Int, Byte] =
    SplitEpi(_.toByte, _.toInt)

  // Laws
  checkAll("Long > Int", SplitEpiTests(ex1).splitEpi)
  checkAll("Int > Byte", SplitEpiTests(ex2).splitEpi)
  checkAll("Long > Int > Byte", SplitEpiTests(ex1.andThen(ex2)).splitEpi)

  test("modify") {
    assertEquals(ex1.modify(_ + 1)(Int.MaxValue.toLong),  Int.MinValue.toLong)
  }

  test("modifyF") {
    assertEquals(ex1.modifyF(i => Option(i + 1))(Int.MaxValue.toLong),  Some(Int.MinValue.toLong))
  }

}
