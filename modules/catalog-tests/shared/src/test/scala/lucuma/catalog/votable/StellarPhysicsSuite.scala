// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.option.*
import munit.FunSuite

class StellarPhysicsSuite extends FunSuite:

  test("letter conversions"):
    assertEquals(StellarPhysics.spectralClassCode("O0"), 0.0.some)
    assertEquals(StellarPhysics.spectralClassCode("B0"), 10.0.some)
    assertEquals(StellarPhysics.spectralClassCode("A0"), 20.0.some)
    assertEquals(StellarPhysics.spectralClassCode("F0"), 30.0.some)
    assertEquals(StellarPhysics.spectralClassCode("G0"), 40.0.some)
    assertEquals(StellarPhysics.spectralClassCode("K0"), 50.0.some)
    assertEquals(StellarPhysics.spectralClassCode("M0"), 60.0.some)
    assertEquals(StellarPhysics.spectralClassCode("L0"), 70.0.some)
    assertEquals(StellarPhysics.spectralClassCode("T0"), 80.0.some)
    assertEquals(StellarPhysics.spectralClassCode("Y0"), 90.0.some)

  test("with decimals"):
    assertEquals(StellarPhysics.spectralClassCode("G3.5"), 43.5.some)
    assertEquals(StellarPhysics.spectralClassCode("K2.5"), 52.5.some)
    assertEquals(StellarPhysics.spectralClassCode("M8.5"), 68.5.some)
    assertEquals(StellarPhysics.spectralClassCode("G3.7"), 43.7.some)
    assertEquals(StellarPhysics.spectralClassCode("K2.3"), 52.3.some)
    assertEquals(StellarPhysics.spectralClassCode("F8.2"), 38.2.some)
    assertEquals(StellarPhysics.spectralClassCode("A5.9"), 25.9.some)

  test("integer subclasses"):
    assertEquals(StellarPhysics.spectralClassCode("G3"), 43.0.some)
    assertEquals(StellarPhysics.spectralClassCode("K2"), 52.0.some)
    assertEquals(StellarPhysics.spectralClassCode("F5"), 35.0.some)

  test("+/- modifiers"):
    assertEquals(StellarPhysics.spectralClassCode("K6-"), 55.75.some)
    assertEquals(StellarPhysics.spectralClassCode("K6"), 56.0.some)
    assertEquals(StellarPhysics.spectralClassCode("K6+"), 56.25.some)
    assertEquals(StellarPhysics.spectralClassCode("K3.5+"), 53.75.some)
    assertEquals(StellarPhysics.spectralClassCode("G2.7-"), 42.45.some)

  test("code defaults to 5 for bare letter"):
    assertEquals(StellarPhysics.spectralClassCode("G"), 45.0.some)
    assertEquals(StellarPhysics.spectralClassCode("K"), 55.0.some)

  test("temperature calculations"):
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("A0")), 11531.some)
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("G2")), 5199.some)
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("K4")), 4321.some)
    assertEquals(StellarPhysics.calculateTemperature(List("III"), List("K1")), 4542.some)
    assertEquals(StellarPhysics.calculateTemperature(List("DA"), List("3")), 16800.some)
    assertEquals(StellarPhysics.calculateTemperature(List("DB"), List("4")), 12600.some)
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("K4+")), 4302.some)
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("K5-")), 4263.some)
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("L5")), None)
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("T8")), None)

  test("temperature calculation averages multiple classes"):
    // B9=12361, A0=11531, average=11946
    assertEquals(StellarPhysics.calculateTemperature(List("V"), List("B9", "A0")), 11946.some)

  test("gravity calculation"):
    // Gravity values from Python's match_sed_log_g.csv table
    assertEquals(StellarPhysics.calculateGravity(List("V"), List("A0")), 4.07.some)
    assertEquals(StellarPhysics.calculateGravity(List("V"), List("G2")), 4.4.some)
    assertEquals(StellarPhysics.calculateGravity(List("III"), List("K1")), 2.78.some)
    assertEquals(StellarPhysics.calculateGravity(List("DA"), List("3")), 8.0.some)
    // I -> Iab
    assertEquals(StellarPhysics.calculateGravity(List("I"), List("A0")), 2.01.some)
    // VI -> sd, interpolated between G0 (sd=3.5) and K0 (sd=3.0)
    assertEquals(StellarPhysics.calculateGravity(List("VI"), List("G2")), 3.4.some)
    // IIIa -> III (drops subclass)
    assertEquals(StellarPhysics.calculateGravity(List("IIIa"), List("A0")), 3.75.some)
