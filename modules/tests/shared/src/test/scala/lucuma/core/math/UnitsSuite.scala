// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb._
import coulomb.mks._
import coulomb.si.Meter
import lucuma.core.math.units._
import munit.FunSuite
import spire.implicits._
import spire.math.Rational
import spire.math.SafeLong

final class UnitsSuite extends FunSuite {

  test("Sanity tests") {
    assert(Rational.one.withUnit[Angstrom] === Rational(1, SafeLong(10).pow(10)).withUnit[Meter])
    assert(
      Rational.one.withUnit[Jansky] === Rational(1, SafeLong(10).pow(26))
        .withUnit[Watt %/ (Meter %^ 2) %* Hertz]
    )
    assert(Rational.one.withUnit[Erg] === Rational(1, SafeLong(10).pow(7)).withUnit[Joule])
  }

  test("Check brightness unit names") {
    assertEquals(1.withUnit[VegaMagnitude].show, "1 Vega mag")
    assertEquals(1.withUnit[ABMagnitude].show, "1 AB mag")
    assertEquals(1.withUnit[Jansky].show, "1 Jy")
    assertEquals(1.withUnit[WattsBrightness].show, "1 W/m²/µm")
    assertEquals(1.withUnit[ErgsWavelengthBrightness].show, "1 erg/s/cm²/Å")
    assertEquals(1.withUnit[ErgsFrequencyBrightness].show, "1 erg/s/cm²/Hz")
  }

}
