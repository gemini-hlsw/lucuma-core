// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import org.junit._
import Assert._

class HoursTest {

  @Test def testFromMillisec() = {
    assertEquals(new Hours(42), Hours.fromMillisec(42L * 60 * 60 * 1000))
  }
}
