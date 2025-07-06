// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.arb.ArbABBA.given
import monocle.law.discipline.LensTests

class ABBASuite extends munit.DisciplineSuite:
  checkAll("Eq[ABBA]", EqTests[ABBA].eqv)
  checkAll("ABBA.a lens", LensTests(ABBA.a))
  checkAll("ABBA.b lens", LensTests(ABBA.b))
  checkAll("ABBA.first lens", LensTests(ABBA.first))
  checkAll("ABBA.second lens", LensTests(ABBA.second))
  checkAll("ABBA.third lens", LensTests(ABBA.third))
  checkAll("ABBA.fourth lens", LensTests(ABBA.fourth))

  val offsetA = Offset(1.arcsec.p, 2.arcsec.q)
  val offsetB = Offset(3.arcsec.p, 4.arcsec.q)

  test("ABBA stores two offsets"):
    val abba = ABBA(offsetA, offsetB)
    assertEquals(abba.a, offsetA)
    assertEquals(abba.b, offsetB)
    val all = abba.all
    assertEquals(all, (offsetA, offsetB, offsetB, offsetA))
    val abba2 = ABBA(offsetA, offsetB, offsetB, offsetA)
    assertEquals(abba2, Some(ABBA(offsetA, offsetB)))

  test("4-offset constructor with completely different offsets"):
    val offsetC = Offset(5.arcsec.p, 6.arcsec.q)
    val offsetD = Offset(7.arcsec.p, 8.arcsec.q)
    val result = ABBA(offsetA, offsetB, offsetC, offsetD)
    assertEquals(result, None)

  test("pattern matching with unapply"):
    val abba = ABBA(offsetA, offsetB)
    abba match
      case ABBA(first, second, third, fourth) =>
        assertEquals(first, offsetA)
        assertEquals(second, offsetB)
        assertEquals(third, offsetB)
        assertEquals(fourth, offsetA)
      case _ => fail("Pattern match should succeed")
