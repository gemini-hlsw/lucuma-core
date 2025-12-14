// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import munit.FunSuite
import coulomb.*

class StellarPhysicsSuite extends FunSuite:

  test("spectral class code handles all letter conversions"):
    assertEquals(StellarPhysics.spectralClassCode("O0"), Some(0.0))
    assertEquals(StellarPhysics.spectralClassCode("B0"), Some(10.0))
    assertEquals(StellarPhysics.spectralClassCode("A0"), Some(20.0))
    assertEquals(StellarPhysics.spectralClassCode("F0"), Some(30.0))
    assertEquals(StellarPhysics.spectralClassCode("G0"), Some(40.0))
    assertEquals(StellarPhysics.spectralClassCode("K0"), Some(50.0))
    assertEquals(StellarPhysics.spectralClassCode("M0"), Some(60.0))
    assertEquals(StellarPhysics.spectralClassCode("L0"), Some(70.0))
    assertEquals(StellarPhysics.spectralClassCode("T0"), Some(80.0))
    assertEquals(StellarPhysics.spectralClassCode("Y0"), Some(90.0))

  test("spectral class code handles standard decimals"):
    assertEquals(StellarPhysics.spectralClassCode("G3.5"), Some(43.5))
    assertEquals(StellarPhysics.spectralClassCode("K2.5"), Some(52.5))
    assertEquals(StellarPhysics.spectralClassCode("M8.5"), Some(68.5))

  test("spectral class code handles non-standard decimals"):
    assertEquals(StellarPhysics.spectralClassCode("G3.7"), Some(43.7))
    assertEquals(StellarPhysics.spectralClassCode("K2.3"), Some(52.3))
    assertEquals(StellarPhysics.spectralClassCode("F8.2"), Some(38.2))
    assertEquals(StellarPhysics.spectralClassCode("A5.9"), Some(25.9))

  test("spectral class code handles integer subclasses"):
    assertEquals(StellarPhysics.spectralClassCode("G3"), Some(43.0))
    assertEquals(StellarPhysics.spectralClassCode("K2"), Some(52.0))
    assertEquals(StellarPhysics.spectralClassCode("F5"), Some(35.0))

  test("spectral class code handles +/- modifiers"):
    assertEquals(StellarPhysics.spectralClassCode("K6-"), Some(55.75))
    assertEquals(StellarPhysics.spectralClassCode("K6"), Some(56.0))
    assertEquals(StellarPhysics.spectralClassCode("K6+"), Some(56.25))

  test("spectral class code handles +/- modifiers with decimals"):
    assertEquals(StellarPhysics.spectralClassCode("K3.5+"), Some(53.75))
    assertEquals(StellarPhysics.spectralClassCode("G2.7-"), Some(42.45))

  test("spectral class code defaults to 5 for bare letter"):
    assertEquals(StellarPhysics.spectralClassCode("G"), Some(45.0))
    assertEquals(StellarPhysics.spectralClassCode("K"), Some(55.0))

  test("temperature calculation for main sequence stars"):
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("A0")), Some(11531))
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("G2")), Some(5199))
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("K4")), Some(4321))

  test("temperature calculation for giants"):
    assertEquals(StellarPhysics.calculateTemperature(List("III"), List("K1")), Some(4542))

  test("temperature calculation for white dwarfs"):
    assertEquals(StellarPhysics.calculateTemperature(List("DA"), List("3")), Some(16800))
    assertEquals(StellarPhysics.calculateTemperature(List("DB"), List("4")), Some(12600))

  test("temperature calculation with +/- modifiers"):
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("K4+")), Some(4302))
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("K5-")), Some(4263))

  test("temperature calculation returns None for brown dwarfs"):
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("L5")), None)
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("T8")), None)

  test("temperature calculation averages multiple classes"):
    // B9=12361, A0=11531, average=11946
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("B9", "A0")), Some(11946))

  test("gravity calculation for main sequence stars"):
    assertEquals(StellarPhysics.calculateGravity(List("V"), List("A0")), Some(4.07))
    assertEquals(StellarPhysics.calculateGravity(List("V"), List("G2")), Some(4.426))

  test("gravity calculation for giants"):
    assertEquals(StellarPhysics.calculateGravity(List("III"), List("K1")), Some(2.764))

  test("gravity calculation for white dwarfs"):
    assertEquals(StellarPhysics.calculateGravity(List("DA"), List("3")), Some(8.0))

  test("gravity calculation normalizes luminosity classes"):
    // I -> Iab
    assertEquals(StellarPhysics.calculateGravity(List("I"), List("A0")), Some(2.01))
    // VI -> sd
    assertEquals(StellarPhysics.calculateGravity(List("VI"), List("G2")), Some(3.4))
    // IIIa -> III (drops subclass)
    assertEquals(StellarPhysics.calculateGravity(List("IIIa"), List("B3")), Some(3.743))

  test("combined parameter calculation"):
    val result = StellarPhysics.calculateParameters(List("V"), List("G2"))
    assert(result.isDefined)
    result.foreach { params =>
      assertEquals(params.tEff.value, 5199)
      assertEquals(params.logG, 4.426)
    }

  test("combined parameter calculation returns None for invalid input"):
    assertEquals(StellarPhysics.calculateParameters(List.empty, List("G2")), None)
    assertEquals(StellarPhysics.calculateParameters(List("V"), List.empty), None)
    assertEquals(StellarPhysics.calculateParameters(List("V"), List("L5")), None)
