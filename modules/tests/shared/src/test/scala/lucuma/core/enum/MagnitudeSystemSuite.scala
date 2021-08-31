// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import munit._
import coulomb._

final class MagnitudeSystemSuite extends FunSuite {

  // Units
  test("Check unit names") {
    assertEquals(1.withUnit[MagnitudeSystem.Vega.Units].show, "1 unitless")
    assertEquals(1.withUnit[MagnitudeSystem.AB.Units].show, "1 unitless")
    assertEquals(1.withUnit[MagnitudeSystem.Jy.Units].show, "1 Jy")
    assertEquals(1.withUnit[MagnitudeSystem.Watts.Units].show, "1 W/(m^2 μm)")
    assertEquals(1.withUnit[MagnitudeSystem.ErgsWavelength.Units].show, "1 erg/(s cm^2 Å)")
    assertEquals(1.withUnit[MagnitudeSystem.ErgsFrequency.Units].show, "1 erg/(s cm^2 Hz)")
  }
}
