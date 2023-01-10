// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom
package syntax

import lucuma.core.math.Angle
import lucuma.core.math.Offset

import ShapeExpression._

final class ShapeExpressionOps(private val self: ShapeExpression) extends AnyVal {

  def difference(that: ShapeExpression): ShapeExpression =
    Difference(self, that)

  def intersection(that: ShapeExpression): ShapeExpression =
    Intersection(self, that)

  def flipP: ShapeExpression =
    FlipP(self)

  def flipQ: ShapeExpression =
    FlipQ(self)

  def rotate(a: Angle): ShapeExpression =
    Rotate(self, a)

  def rotateAroundOffset(a: Angle, o: Offset): ShapeExpression =
    RotateAroundOffset(self, a, o)

  def translate(o: Offset): ShapeExpression =
    Translate(self, o)

  def union(that: ShapeExpression): ShapeExpression =
    Union(self, that)

  def -(that: ShapeExpression): ShapeExpression =
    difference(that)

  def ∩(that: ShapeExpression): ShapeExpression =
    intersection(that)

  def ⟲(a: Angle): ShapeExpression =
    rotate(a)

  def ⟲(a: Angle, o: Offset): ShapeExpression =
    rotateAroundOffset(a, o)

  def ↗(o: Offset): ShapeExpression =
    translate(o)

  def ∪(that: ShapeExpression): ShapeExpression =
    union(that)

  /**
   * Creates a bounding box for an expression
   */
  def boundingBox: ShapeExpression =
    BoundingBox(self)

  /**
   * Promotes `Shape.contains` through evaluation.
   */
  def contains(o: Offset)(implicit ev: ShapeInterpreter): Boolean =
    self.eval.contains(o)

  /**
   * Promotes `Shape.area` through evaluation.
   */
  def area(implicit ev: ShapeInterpreter): Area =
    self.eval.area

  /**
   * Promotes `Shape.area` through evaluation and produces µas^2^ as a `Long`.
   */
  def µasSquared(implicit ev: ShapeInterpreter): Long =
    area.toMicroarcsecondsSquared

  /**
   * Promotes `Shape.boundingOffsets.maxSide` through evaluation and produces an Angle
   */
  def maxSide(implicit ev: ShapeInterpreter): Angle =
    self.eval.boundingOffsets.maxSide

}

trait ToShapeExpressionOps {
  implicit def ToShapeExpressionOps(e: ShapeExpression): ShapeExpressionOps =
    new ShapeExpressionOps(e)
}

final class ShapeExpressionCompanionOps(private val self: ShapeExpression.type) extends AnyVal {

  /**
   * An empty `ShapeExpression` with a broader type.
   */
  def empty: ShapeExpression =
    ShapeExpression.Empty

  /**
   * An single point `ShapeExpression`
   */
  def point(a: Offset): ShapeExpression =
    ShapeExpression.Point(a)

  /**
   * Constructs an ellipse bound by the rectangle defined by the given coordinates expressed as
   * angular separation.
   *
   * @group Constructors
   */
  def ellipseAt(a: (Offset.P, Offset.Q), b: (Offset.P, Offset.Q)): ShapeExpression =
    Ellipse(Offset(a._1, a._2), Offset(b._1, b._2))

  /**
   * Constructs an arbitrary polygon defined by the given coordinate list expressed as angular
   * separation.
   *
   * @group Constructors
   */
  def polygonAt(os: (Offset.P, Offset.Q)*): ShapeExpression =
    Polygon(os.toList.map(o => Offset(o._1, o._2)))

  /**
   * Constructs a rectangle bound by the two given coordinates expressed as angular separation.
   *
   * @group Constructors
   */
  def rectangleAt(a: (Offset.P, Offset.Q), b: (Offset.P, Offset.Q)): ShapeExpression =
    Rectangle(Offset(a._1, a._2), Offset(b._1, b._2))

  /**
   * Constructs a regular polygon of a given radius and number of sides, centered at the origin with
   * a vertex at (radius, 0).
   *
   * @group Constructors
   */
  def regularPolygon(radius: Angle, nSides: Int): ShapeExpression = {
    val µas = radius.toMicroarcseconds
    val v0  = radius.offsetInP
    val vs  = (1 until nSides).foldLeft(List(v0)) { (os, v) =>
      val θ = 2 * v * Math.PI / nSides.toDouble
      Offset.signedMicroarcseconds.reverseGet(
        (
          (µas * Math.cos(θ)).round,
          (µas * Math.sin(θ)).round
        )
      ) :: os
    }
    Polygon(vs)
  }

  /**
   * Constructs a rectangle of width w and height h with a corner at (0,0).
   *
   * @group Constructors
   */
  def rectangle(w: Angle, h: Angle): ShapeExpression =
    Rectangle(Offset.Zero, Offset(w.p, h.q))

  /**
   * Constructs a rectangle of width w and height h centered at the base position, to within
   * microarcsecond precision.
   *
   * @group Constructors
   */
  def centeredRectangle(w: Angle, h: Angle): ShapeExpression =
    Translate(rectangle(w, h), Offset(-w.bisect.p, -h.bisect.q))

  /**
   * Constructs an ellipse contained in a rectangle of width w and height h with a corner at (0,0).
   *
   * @group Constructors
   */
  def ellipse(w: Angle, h: Angle): ShapeExpression =
    Ellipse(Offset.Zero, Offset(w.p, h.q))

  /**
   * Constructs an ellipse contained in a rectangle of width w and height centered at the base
   * position, to within microarcsecond precision.
   *
   * @group Constructors
   */
  def centeredEllipse(w: Angle, h: Angle): ShapeExpression =
    Translate(ellipse(w, h), Offset(-w.bisect.p, -h.bisect.q))

}

trait ToShapeExpressionCompanionOps {
  implicit def ToShapeExpressionCompanionOps(c: ShapeExpression.type): ShapeExpressionCompanionOps =
    new ShapeExpressionCompanionOps(c)
}

object shapeexpression extends ToShapeExpressionOps with ToShapeExpressionCompanionOps
