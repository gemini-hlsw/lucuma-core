// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import org.junit._
import Assert._
import edu.gemini.spModel.core.Site
import edu.gemini.spModel.core.Semester
import scala.jdk.CollectionConverters._

class ExcelRaBinCalcTest {

  @Test def testKnownGood() {
    val calc = RaDecBinCalc.get(Site.GS, Semester.parse("2020A"), new RaBinSize(3 * 60), new DecBinSize(20))
    val hrs  = calc.getRaHours.asScala.map(_.getHours).toList
    val exp  = List(54.0, 0.0, 183.0, 354.0, 477.0, 423.0, 306.0, 183.0)
    assertEquals(exp, hrs)
  }

}
