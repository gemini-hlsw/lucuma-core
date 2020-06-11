// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{Eq, Show}
import cats.kernel.laws.discipline._
import gsp.math.arb._
import gsp.math.laws.discipline.SplitMonoTests
import gsp.math.syntax.int._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class OffsetSpec extends CatsSuite {
  import ArbAngle._
  import ArbOffset._

  // Laws
  checkAll("Offset", CommutativeGroupTests[Offset].commutativeGroup)
  checkAll("Offset", OrderTests[Offset].order)
  checkAll("Axis.P", LensTests(Offset.p))
  checkAll("Axis.Q", LensTests(Offset.q))
  checkAll("Axis.PAngle", LensTests(Offset.pAngle))
  checkAll("Axis.QAngle", LensTests(Offset.qAngle))
  checkAll("Offset.microarcseconds", SplitMonoTests(Offset.microarcseconds).splitMono)
  checkAll("Offset.signedMicroarcseconds", SplitMonoTests(Offset.signedMicroarcseconds).splitMono)
  checkAll("Offset.signedArcseconds", SplitMonoTests(Offset.signedArcseconds).splitMono)

  test("Equality must be natural") {
    forAll { (a: Offset, b: Offset) =>
      a.equals(b) shouldEqual Eq[Offset].eqv(a, b)
    }
  }

  test("it must operate pairwise") {
    forAll { (a: Offset, b: Offset) =>
      Eq[Offset.Component[Axis.P]].eqv(a.p, b.p) &&
      Eq[Offset.Component[Axis.Q]].eqv(a.q, b.q) shouldEqual Eq[Offset].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { a: Offset =>
      a.toString shouldEqual Show[Offset].show(a)
    }
  }

  test("Conversion to components must be invertable") {
    forAll { o: Offset =>
      val (p, q) = (o.p, o.q)
      Offset(p, q) shouldEqual o
    }
  }

  test("subtraction is addition with unary_-") {
    forAll { (a: Offset, b: Offset) =>
      (a - b) shouldEqual (a + -b)
    }
  }

  test("Fixed Rotation tests") {
    val pqa = List(
      (-1.arcsec.p,  0.arcsec.q,  90.deg) -> (( 0.arcsec.p,  1.arcsec.q)),
      ( 0.arcsec.p,  1.arcsec.q,  90.deg) -> (( 1.arcsec.p,  0.arcsec.q)),
      ( 1.arcsec.p,  0.arcsec.q,  90.deg) -> (( 0.arcsec.p, -1.arcsec.q)),
      ( 0.arcsec.p, -1.arcsec.q,  90.deg) -> ((-1.arcsec.p,  0.arcsec.q)),
      (-1.arcsec.p,  0.arcsec.q, -90.deg) -> (( 0.arcsec.p, -1.arcsec.q)),
      ( 0.arcsec.p,  1.arcsec.q, -90.deg) -> ((-1.arcsec.p,  0.arcsec.q)),
      ( 1.arcsec.p,  0.arcsec.q, -90.deg) -> (( 0.arcsec.p,  1.arcsec.q)),
      ( 0.arcsec.p, -1.arcsec.q, -90.deg) -> (( 1.arcsec.p,  0.arcsec.q)),
      (-1.arcsec.p,  0.arcsec.q,  30.deg) -> ((-866025.µas.p,  500000.µas.q)),
      (-1.arcsec.p,  0.arcsec.q, -30.deg) -> ((-866025.µas.p, -500000.µas.q)),
      (-2999291.µas.p,  9537000.µas.q,  Angle.fromDMS(148, 0, 54, 775, 807)) -> (( 7595657.µas.p, -6500470.µas.q)),
      ( 7595657.µas.p, -6500470.µas.q, -Angle.fromDMS(148, 0, 54, 775, 807)) -> ((-2999291.µas.p,  9537000.µas.q))
    )

    pqa.foreach { case ((p, q, θ), (pʹ, qʹ)) =>
      val expected = Offset(pʹ, qʹ)
      val actual = Offset(p, q).rotate(θ)
      if (Eq[Offset].neqv(expected, actual)) {
        val t = s"Offset(${Offset.P.signedArcseconds.get(p)}, ${Offset.Q.signedArcseconds.get(q)}).rotate(${θ.toSignedDoubleDegrees}º)"

        fail(
          s"""$t
             |  Expected: $expected
             |  Actual..: $actual
        """.stripMargin)
      }
    }
  }

  test("Rotation is invertable within 1 µas") {
    // not exactly invertable because of rounding
    forAll { (o: Offset, θ: Angle) =>
      val oʹ = o.rotate(θ).rotate(-θ)
      val (p,  q)  = Offset.signedMicroarcseconds.get(o)
      val (pʹ, qʹ) = Offset.signedMicroarcseconds.get(oʹ)
      assert(
        ((p - pʹ).abs <= 1) && ((q - qʹ).abs <= 1)
      )
    }
  }
}
