// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import munit.FunSuite

class ExcelRaBinCalcTest extends FunSuite {

  test("testKnownGood") {
    val calc = RaDecBinCalc.get(Site.GS, Semester(YearInt.unsafeFrom(2020), Half.A), RaBinSize.ofArcMinutes(3 * 60).get, DecBinSize.ofDegrees(20).get)
    val hrs  = calc.raHours.map(_.hours).toList
    val exp  = List(54.0, 0.0, 183.0, 354.0, 477.0, 423.0, 306.0, 183.0)
    // println(s"calculated decPercents ${calc.decPercents}")
    // println(s"calculated $hrs")
    // println(s"expected   $exp")
    assertEquals(hrs, exp)
  }

}
