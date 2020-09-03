// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.tests.CatsSuite
import lucuma.core.math.{ Angle, Offset }
import lucuma.core.geom.syntax.all._
import lucuma.core.math.arb._
import lucuma.core.math.syntax.int._
import lucuma.core.geom.arb._
import org.scalacheck._
import org.scalacheck.Arbitrary._

final class ShapeExpressionSpec extends CatsSuite {
  implicit val interpreter: ShapeInterpreter =
    lucuma.core.geom.jts.interpreter.value

  import ArbAngle._
  import ArbOffset._
  import ArbShapeExpression._
  import ShapeExpressionSpec._

  test("intersection contains") {
    forAll(genTwoCenteredShapesAndAnOffset) {
      case (tcs, off) =>
        (tcs.shape0 ∩ tcs.shape1).contains(off) shouldEqual (
          tcs.shape0.contains(off) && tcs.shape1.contains(off)
        )
    }
  }

  test("union contains") {
    forAll(genTwoCenteredShapesAndAnOffset) {
      case (tcs, off) =>
        (tcs.shape0 ∪ tcs.shape1).contains(off) shouldEqual (
          tcs.shape0.contains(off) || tcs.shape1.contains(off)
        )
    }
  }

  test("difference contains") {
    forAll(genTwoCenteredShapesAndAnOffset) {
      case (tcs, off) =>
        (tcs.shape0 - tcs.shape1).contains(off) shouldEqual (
          tcs.shape0.contains(off) && !tcs.shape1.contains(off)
        )
    }
  }

  test("(a ∪ b).area = (a.area + b.area) - (a ∩ b).area") {
    forAll(genTwoCenteredShapes) { tcs =>
      val rhs = (tcs.shape0 ∪ tcs.shape1).µasSquared
      val lhs = (tcs.shape0.µasSquared + tcs.shape1.µasSquared) -
        (tcs.shape0 ∩ tcs.shape1).µasSquared

      // Area calculation isn't exact but within 1/2 mas^2 seems fine for our
      // purposes.
      (rhs - lhs) shouldEqual 0L +- 500L
    }
  }

  test("(a ∩ b).area = (a ∪ b).area - ((a - b).area + (b - a).area)") {
    forAll(genTwoCenteredShapes) { tcs =>
      val rhs = (tcs.shape0 ∩ tcs.shape1).µasSquared
      val lhs = (tcs.shape0 ∪ tcs.shape1).µasSquared - (
        (tcs.shape0 - tcs.shape1).µasSquared +
          (tcs.shape1 - tcs.shape0).µasSquared
      )

      // Area calculation isn't exact but within 1/2 mas^2 seems fine.
      (rhs - lhs) shouldEqual 0L +- 500L
    }
  }

  // There is a bug apparently in JTS that makes the area calculation a bit off
  // after rotation and/or translation in some cases.  This is expressed as a
  // maximum fraction of the nominal area of the original shape.

  test("area is the 'same' after rotation") {
    forAll(genShape, arbitrary[Angle]) { (e: ShapeExpression, a) =>
      val nominal = e.µasSquared
      val rotated = (e ⟲ a).µasSquared
      val error   = if (nominal === 0L) 0L else (nominal - rotated).toDouble / nominal.toDouble
      error shouldEqual 0.0 +- 1.0e-13
    }
  }

  test("area is the 'same' after translation") {
    forAll(genShape, arbitrary[Offset]) { (e: ShapeExpression, o) =>
      val nominal = e.µasSquared
      val moved   = (e ↗ o).µasSquared
      val error   = if (nominal === 0L) 0L else (nominal - moved).toDouble / nominal.toDouble
      error shouldEqual 0.0 +- 1.0e-13
    }
  }

  test("intersection with a fully contained shape") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.choose(3, 10)) { (r0, r1, s) =>
      val p0 = ShapeExpression.regularPolygon(r0.arcsec, s)
      val p1 = ShapeExpression.regularPolygon(r1.arcsec, s)
      val p  = if (r0 <= r1) p0 else p1

      p.area shouldEqual (p0 ∩ p1).area
    }
  }

  test("union with a fully contained shape") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.choose(3, 10)) { (r0, r1, s) =>
      val p0 = ShapeExpression.regularPolygon(r0.arcsec, s)
      val p1 = ShapeExpression.regularPolygon(r1.arcsec, s)
      val p  = if (r0 >= r1) p0 else p1

      p.area shouldEqual (p0 ∪ p1).area
    }
  }

  test("difference with a fully contained shape") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.choose(3, 10)) { (r0, r1, s) =>
      val p0 = ShapeExpression.regularPolygon(r0.arcsec, s)
      val p1 = ShapeExpression.regularPolygon(r1.arcsec, s)
      val p  = if (r0 >= r1) p0 - p1 else p1 - p0

      (p0.µasSquared - p1.µasSquared).abs shouldBe p.µasSquared
    }
  }
}

object ShapeExpressionSpec {

  import lucuma.core.geom.arb.ArbShapeExpression.genCenteredShapeOf

  case class TwoCenteredShapes(
    radius0: Angle,
    shape0:  ShapeExpression,
    radius1: Angle,
    shape1:  ShapeExpression
  ) {
    val minRadius: Angle =
      Angle.AngleOrder.min(radius0, radius1)

    val maxRadius: Angle =
      Angle.AngleOrder.max(radius0, radius1)
  }

  val genTwoCenteredShapes: Gen[TwoCenteredShapes] =
    for {
      r0 <- Gen.choose(1, 400).map(_.arcsec)
      s0 <- genCenteredShapeOf(r0)
      r1 <- Gen.choose(1, 400).map(_.arcsec)
      s1 <- genCenteredShapeOf(r1)
    } yield TwoCenteredShapes(r0, s0, r1, s1)

  val genTwoCenteredShapesAndAnOffset: Gen[(TwoCenteredShapes, Offset)] =
    for {
      t <- genTwoCenteredShapes
      p <- Gen.choose(0L, t.maxRadius.toMicroarcseconds).map(Angle.fromMicroarcseconds)
      q <- Gen.choose(0L, t.maxRadius.toMicroarcseconds).map(Angle.fromMicroarcseconds)
    } yield (t, Offset(Offset.P(p), Offset.Q(q)))

}
