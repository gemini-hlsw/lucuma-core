// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.syntax.all.*
import lucuma.core.geom.ShapeExpression.*
import lucuma.core.geom.arb.*
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Offset.Component
import lucuma.core.math.arb.*
import lucuma.core.math.syntax.int.*
import lucuma.core.tests.ScalaCheckFlaky
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import org.scalacheck.*

final class ShapeExpressionSuite extends munit.DisciplineSuite {

  // Override to remove implicit modifier
  override def unitToProp = super.unitToProp
  // Scala 3 likes this better
  implicit def saneUnitToProp(unit: Unit): Prop = unitToProp(unit)

  import ArbAngle.given
  import ArbOffset.given
  import ArbShapeExpression.*
  import ShapeExpressionSpec.*

  test("intersection contains") {
    forAll(genTwoCenteredShapesAndAnOffset) { case (tcs, off) =>
      assertEquals(
        (tcs.shape0 ∩ tcs.shape1).contains(off),
        tcs.shape0.contains(off) && tcs.shape1.contains(off)
      )
    }
  }

  test("union contains") {
    forAll(genTwoCenteredShapesAndAnOffset) { case (tcs, off) =>
      assertEquals(
        (tcs.shape0 ∪ tcs.shape1).contains(off),
        tcs.shape0.contains(off) || tcs.shape1.contains(off)
      )
    }
  }

  test("max side of the bounding box is bigger than for each shape") {
    implicit val order = Angle.SignedAngleOrder
    forAll(genTwoCenteredShapes) { case shapes =>
      assert(
        (shapes.shape0 ∪ shapes.shape1).maxSide >= shapes.shape0.maxSide
      )
      assert(
        (shapes.shape0 ∪ shapes.shape1).maxSide >= shapes.shape1.maxSide
      )
    }
  }

  test("difference contains") {
    forAll(genTwoCenteredShapesAndAnOffset) { case (tcs, off) =>
      assertEquals(
        (tcs.shape0 - tcs.shape1).contains(off),
        tcs.shape0.contains(off) && !tcs.shape1.contains(off)
      )
    }
  }

  test("(a ∪ b).area = (a.area + b.area) - (a ∩ b).area".tag(ScalaCheckFlaky)) {
    forAll(genTwoCenteredShapes) { tcs =>
      val rhs = (tcs.shape0 ∪ tcs.shape1).µasSquared
      val lhs = (tcs.shape0.µasSquared + tcs.shape1.µasSquared) -
        (tcs.shape0 ∩ tcs.shape1).µasSquared

      // Area calculation isn't exact but within 1/2 mas^2 seems fine for our
      // purposes.
      assertEqualsDouble((rhs - lhs).toDouble, 0L, 700L)
    }
  }

  test("(a ∩ b).area = (a ∪ b).area - ((a - b).area + (b - a).area)".tag(ScalaCheckFlaky)) {
    forAll(genTwoCenteredShapes) { tcs =>
      val rhs = (tcs.shape0 ∩ tcs.shape1).µasSquared
      val lhs = (tcs.shape0 ∪ tcs.shape1).µasSquared - (
        (tcs.shape0 - tcs.shape1).µasSquared +
          (tcs.shape1 - tcs.shape0).µasSquared
      )

      // Area calculation isn't exact but within 1/2 mas^2 seems fine.
      assertEqualsDouble((rhs - lhs).toDouble, 0L, 700L)
    }
  }

  test("bounding box area contains the shape") {
    forAll(genShape) { (e: ShapeExpression) =>
      val error = e.boundingBox.µasSquared - e.µasSquared

      // Area calculation isn't exact but within 1/2 mas^2 seems fine.
      assert(error >= 0 || error.toDouble < -1.0e-13)
    }
  }

  test("bounding box area contains two unioned shapes") {
    forAll(genShape, genShape) { (a: ShapeExpression, b: ShapeExpression) =>
      val error = (a ∪ b).boundingBox.µasSquared - (a ∪ b).µasSquared

      // Area calculation isn't exact but within 1/2 mas^2 seems fine.
      assert(error >= 0 || error.toDouble < -1.0e-13)
    }
  }


  // There is a bug apparently in JTS that makes the area calculation a bit off
  // after rotation and/or translation in some cases.  This is expressed as a
  // maximum fraction of the nominal area of the original shape.

  test("area is the 'same' after rotation") {
    forAll(genShape, arbitrary[Angle]) { (e: ShapeExpression, a) =>
      val nominal = e.µasSquared
      val rotated = (e ⟲ a).µasSquared
      val error   = if (nominal === 0L) 0.0 else (nominal - rotated).toDouble / nominal.toDouble
      assertEqualsDouble(error, 0.0, 1.0e-13)
    }
  }

  test("area is the 'same' after translation") {
    forAll(genShape, arbitrary[Offset]) { (e: ShapeExpression, o) =>
      val nominal = e.µasSquared
      val moved   = (e ↗ o).µasSquared
      val error   = if (nominal === 0L) 0.0 else (nominal - moved).toDouble / nominal.toDouble
      assertEqualsDouble(error, 0.0, 1.0e-13)
    }
  }

  test("intersection with a fully contained shape") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.choose(3, 10)) { (r0, r1, s) =>
      val p0 = ShapeExpression.regularPolygon(r0.arcsec, s)
      val p1 = ShapeExpression.regularPolygon(r1.arcsec, s)
      val p  = if (r0 <= r1) p0 else p1

      val nominal = p.area.toMicroarcsecondsSquared
      val intersected = (p0 ∩ p1).area.toMicroarcsecondsSquared
      val error   = if (nominal === 0L) 0.0 else (nominal - intersected).toDouble / nominal.toDouble
      assertEqualsDouble(error, 0.0, 1.0e-13)
    }
  }

  test("union with a fully contained shape") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.choose(3, 10)) { (r0, r1, s) =>
      val p0 = ShapeExpression.regularPolygon(r0.arcsec, s)
      val p1 = ShapeExpression.regularPolygon(r1.arcsec, s)
      val p  = if (r0 >= r1) p0 else p1

      val nominal = p.area.toMicroarcsecondsSquared
      val united = (p0 ∪ p1).area.toMicroarcsecondsSquared
      val error   = if (nominal === 0L) 0.0 else (nominal - united).toDouble / nominal.toDouble
      assertEqualsDouble(error, 0.0, 1.0e-13)
    }
  }

  test("difference with a fully contained shape") {
    forAll(Gen.choose(1, 10), Gen.choose(1, 10), Gen.choose(3, 10)) { (r0, r1, s) =>
      (r0 > 0 && r1 > 0) ==> {
        val p0 = ShapeExpression.regularPolygon(r0.arcsec, s)
        val p1 = ShapeExpression.regularPolygon(r1.arcsec, s)
        val p  = if (r0 >= r1) p0 - p1 else p1 - p0

        assertEquals((p0.µasSquared - p1.µasSquared).abs, p.µasSquared)
      }
    }
  }

  test("Regression test") {
    val l = List[ShapeExpression](
      Rotate(
        e = Translate(
          e = FlipQ(
            e = Translate(
              e = Translate(
                e = Rectangle(
                  a = Offset(p = Component(Angle.fromMicroarcseconds(0L)), q = Component(Angle.fromMicroarcseconds(0L))),
                  b = Offset(p = Component(Angle.fromMicroarcseconds(212700000L)), q = Component(Angle.fromMicroarcseconds(249600000L)))
                ),
                o = Offset(
                  p = Component(Angle.fromMicroarcseconds(1295893650000L)),
                  q = Component(Angle.fromMicroarcseconds(1295875200000L))
                )
              ),
              o = Offset(
                p = Component(Angle.fromMicroarcseconds(1295905050000L)),
                q = Component(Angle.fromMicroarcseconds(1295910120000L))
              )
            )
          ),
          o = Offset(p = Component(Angle.fromMicroarcseconds(0L)), q = Component(Angle.fromMicroarcseconds(0L)))
        ),
        a = Angle.fromMicroarcseconds(288000000000L)
      ),
      Rotate(
        e = Translate(
          e = FlipQ(
            e = Translate(
              e = Translate(
                e = Rectangle(
                  a = Offset(p = Component(Angle.fromMicroarcseconds(0L)), q = Component(Angle.fromMicroarcseconds(0L))),
                  b = Offset(p = Component(Angle.fromMicroarcseconds(212700000L)), q = Component(Angle.fromMicroarcseconds(249600000L)))
                ),
                o = Offset(
                  p = Component(Angle.fromMicroarcseconds(1295893650000L)),
                  q = Component(Angle.fromMicroarcseconds(1295875200000L))
                )
              ),
              o = Offset(
                p = Component(Angle.fromMicroarcseconds(1295905050000L)),
                q = Component(Angle.fromMicroarcseconds(1295910120000L))
              )
            )
          ),
          o = Offset(p = Component(Angle.fromMicroarcseconds(0L)), q = Component(Angle.fromMicroarcseconds(15000000L)))
        ),
        a = Angle.fromMicroarcseconds(288000000000L)
      ),
      Rotate(
        e = Translate(
          e = FlipQ(
            e = Translate(
              e = Translate(
                e = Rectangle(
                  a = Offset(p = Component(Angle.fromMicroarcseconds(0L)), q = Component(Angle.fromMicroarcseconds(0L))),
                  b = Offset(p = Component(Angle.fromMicroarcseconds(212700000L)), q = Component(Angle.fromMicroarcseconds(249600000L)))
                ),
                o = Offset(
                  p = Component(Angle.fromMicroarcseconds(1295893650000L)),
                  q = Component(Angle.fromMicroarcseconds(1295875200000L))
                )
              ),
              o = Offset(
                p = Component(Angle.fromMicroarcseconds(1295905050000L)),
                q = Component(Angle.fromMicroarcseconds(1295910120000L))
              )
            )
          ),
          o = Offset(p = Component(Angle.fromMicroarcseconds(10000000L)), q = Component(Angle.fromMicroarcseconds(0L)))
        ),
        a = Angle.fromMicroarcseconds(288000000000L)
      )
    )
    // This operation was failing with the previous JTS library port
    assert(l.reduce(_ ∩ _).eval != null)
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
