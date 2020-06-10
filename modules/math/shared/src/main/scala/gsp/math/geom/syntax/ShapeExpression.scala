// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom
package syntax

import ShapeExpression._

import gsp.math.{Angle, Offset}


final class ShapeExpressionOps(val self: ShapeExpression) extends AnyVal {

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
}

trait ToShapeExpressionOps {
  implicit def ToShapeExpressionOps(e: ShapeExpression): ShapeExpressionOps =
    new ShapeExpressionOps(e)
}

final class ShapeExpressionCompanionOps(val self: ShapeExpression.type) extends AnyVal {

  /**
   * An empty `ShapeExpression` with a broader type.
   */
  def empty: ShapeExpression =
    ShapeExpression.Empty

  // Simplified constructors

  private def offset(p: Angle, q: Angle): Offset =
    Offset(Offset.P(p), Offset.Q(q))

  private def offset(o: (Angle, Angle)): Offset =
    Offset(Offset.P(o._1), Offset.Q(o._2))

  /**
   * Constructs an ellipse bound by the rectangle defined by the given
   * coordinates expressed as angular separation.
   *
   * @group Constructors
   */
  def ellipseAt(a: (Angle, Angle), b: (Angle, Angle)): ShapeExpression =
    Ellipse(offset(a), offset(b))

  /**
   * Constructs an arbitrary polygon defined by the given coordinate list
   * expressed as angular separation.
   *
   * @group Constructors
   */
  def polygonAt(os: (Angle, Angle)*): ShapeExpression =
    Polygon(os.toList.map(offset))

  /**
   * Constructs a rectangle bound by the two given coordinates expressed as
   * angular separation.
   *
   * @group Constructors
   */
  def rectangleAt(a: (Angle, Angle), b: (Angle, Angle)): ShapeExpression =
    Rectangle(offset(a), offset(b))

  /**
   * Constructs a regular polygon of a given radius and number of sides,
   * centered at the origin with a vertex at (radius, 0).
   *
   * @group Constructors
   */
  def regularPolygon(radius: Angle, nSides: Int): ShapeExpression = {
    val µas = radius.toMicroarcseconds
    val v0  = Offset(Offset.P(radius), Offset.Q.Zero)
    val vs  = (1 until nSides).foldLeft(List(v0)) { (os, v) =>
      val θ = 2*v*Math.PI/nSides.toDouble
      val p = Angle.signedMicroarcseconds.reverseGet((µas * Math.cos(θ)).round).p
      val q = Angle.signedMicroarcseconds.reverseGet((µas * Math.sin(θ)).round).q
      Offset(p, q) :: os
    }
    Polygon(vs)
  }

  /**
   * Constructs a rectangle of width w and height h with a corner at (0,0).
   *
   * @group Constructors
   */
  def rectangle(w: Angle, h: Angle): ShapeExpression =
    Rectangle(Offset.Zero, offset(w, h))

  /**
   * Constructs a rectangle of width w and height h centered at the base
   * position, to within microarcsecond precision.
   *
   * @group Constructors
   */
  def centeredRectangle(w: Angle, h: Angle): ShapeExpression =
    Translate(rectangle(w, h), offset(-w.bisect, -h.bisect))

  /**
   * Constructs an ellipse contained in a rectangle of width w and height h with
   * a corner at (0,0).
   *
   * @group Constructors
   */
  def ellipse(w: Angle, h: Angle): ShapeExpression =
    Ellipse(Offset.Zero, offset(w, h))

  /**
   * Constructs an ellipse contained in a rectangle of width w and height
   * centered at the base position, to within microarcsecond precision.
   *
   * @group Constructors
   */
  def centeredEllipse(w: Angle, h: Angle): ShapeExpression =
    Translate(ellipse(w, h), offset(-(w.bisect), -(h.bisect)))

}

trait ToShapeExpressionCompanionOps {
  implicit def ToShapeExpressionCompanionOps(c: ShapeExpression.type): ShapeExpressionCompanionOps =
    new ShapeExpressionCompanionOps(c)
}

object shapeexpression extends ToShapeExpressionOps with ToShapeExpressionCompanionOps

