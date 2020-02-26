// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.optics

import cats.tests.CatsSuite
import gsp.math.laws.discipline._

final class SplitMonoSpec extends CatsSuite {

  val ex1: SplitMono[Byte, Int] =
    SplitMono(_.toInt, _.toByte)

  val ex2: SplitMono[Int, Long] =
    SplitMono(_.toLong, _.toInt)

  // Laws
  checkAll("Byte < Int", SplitMonoTests(ex1).splitMono)
  checkAll("Int < Long", SplitMonoTests(ex2).splitMono)
  checkAll("Byte < Int < Long", SplitMonoTests(ex1 composeSplitMono ex2).splitMono)

  test("modify") {
    ex1.modify(_ + 1)(Byte.MaxValue) shouldEqual Byte.MinValue
  }

  test("modifyF") {
    ex1.modifyF(i => Option(i + 1))(Byte.MaxValue) shouldEqual Some(Byte.MinValue)
  }

}
