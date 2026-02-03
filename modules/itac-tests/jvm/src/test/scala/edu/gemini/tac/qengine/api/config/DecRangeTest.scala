// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.Target
import munit.FunSuite
import org.junit.*

import Assert.*

class DecRangeTest extends FunSuite {

  val rng = DecRange(0, 10)
  val irng = DecRange.inclusive(0, 10)

  test("testDefaultIsInclusive") {
    assertFalse(rng.isInclusive)
  }

  test("testInclusiveIsInclusive") {
    assertTrue(irng.isInclusive)
  }

  test("testInclusive") {
    assertTrue(rng.inclusive.isInclusive)
    assertTrue(rng.inclusive.contains(Target(0, 10.0)))
    assertSame(irng, irng.inclusive)
  }

  private def validateContains(r: DecRange) = {
    assertTrue(r.contains(Target(0, 0)))
    assertTrue(r.contains(Target(0, 5.0)))
    assertTrue(r.contains(Target(0, 9.99)))
  }

  test("testDefaultContains") {
    validateContains(rng)
    assertFalse(rng.contains(Target(0, 10.0)))
  }

  test("testInclusiveContains") {
    validateContains(irng)
    assertTrue(irng.contains(Target(0, 10.0)))
    assertFalse(irng.contains(Target(0, 10.001)))
  }

  test("testAbutsRight") {
    assertTrue(rng.abutsRight(DecRange(10, 20)))
    assertFalse(irng.abutsRight(DecRange(10, 20)))

    assertFalse(rng.abutsRight(DecRange(11, 20)))
    assertFalse(rng.abutsRight(DecRange(9, 20)))
    assertFalse(rng.abutsRight(DecRange(-10, 0)))

    assertTrue(DecRange(-10, 0).abutsRight(rng))
  }

  test("testEquals") {
    assertEquals(rng, DecRange(0, 10))
    assertFalse(rng.equals(irng))
    assertFalse(rng.equals(DecRange(1, 10)))
    assertFalse(rng.equals(DecRange(0,  9)))
  }

  test("testValidateNil") {
    assertTrue(DecRange.validate(Nil))
  }

  test("testValidateOne") {
    assertTrue(DecRange.validate(List(rng)))
  }

  test("testValidateTwo") {
    assertTrue(DecRange.validate(List(rng, DecRange(10, 20))))
  }

  test("testValidateThree") {
    assertTrue(DecRange.validate(List(rng, DecRange(10, 20), DecRange(20, 30))))
  }

  test("testInclusiveTerminate") {
    assertTrue(DecRange.validate(List(rng, DecRange.inclusive(10, 20))))
  }

  test("testGap") {
    assertFalse(DecRange.validate(List(rng, DecRange(11, 20))))
  }

  test("testOverlap") {
    assertFalse(DecRange.validate(List(rng, DecRange(9, 20))))
  }

  test("testOrder") {
    assertFalse(DecRange.validate(List(DecRange(10, 20), rng)))
  }
}