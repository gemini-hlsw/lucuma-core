// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import lucuma.core.math.{ Angle, Offset }

/**
  * Describes a `Shape`, which is produced by evaluating the expression using
  * the JVM or JavaScript specific interpreter.
  */
sealed trait ShapeExpression {

  /**
    * Evaluates the expression into a `Shape`, assuming an interpreter is in
    * scope.
    *
    * @param ev shape interpreter to use
    *
    * @return Shape implementation
    */
  def eval(implicit ev: ShapeInterpreter): Shape =
    ev.interpret(this)

}

object ShapeExpression {

  /**
    * An empty shape for use in building up an expression iteratively.
    *
    * @group Constructors
    */
  final case object Empty extends ShapeExpression

  /**
    * Ellipse contained in the rectangle defined by the two positions.
    *
    * @group Constructors
    */
  final case class Ellipse(a: Offset, b: Offset) extends ShapeExpression

  /**
    * Polygon defined by a list of offset positions.
    *
    * @group Constructors
    */
  final case class Polygon(os: List[Offset]) extends ShapeExpression

  /**
    * Rectangle defined by two offset positions.
    *
    * @group Constructors
    */
  final case class Rectangle(a: Offset, b: Offset) extends ShapeExpression

  /**
    * @group Combinations
    */
  final case class Difference(a: ShapeExpression, b: ShapeExpression) extends ShapeExpression

  /**
    * @group Combinations
    */
  final case class Intersection(a: ShapeExpression, b: ShapeExpression) extends ShapeExpression

  /**
    * @group Combinations
    */
  final case class Union(a: ShapeExpression, b: ShapeExpression) extends ShapeExpression

  /**
    * @group Transformations
    */
  final case class Rotate(e: ShapeExpression, a: Angle) extends ShapeExpression

  /**
    * Rotates the shape around the point at the given offset rather than the
    * origin.
    *
    * @group Transformations
    */
  final case class RotateAroundOffset(e: ShapeExpression, a: Angle, o: Offset)
      extends ShapeExpression

  /**
    * Flips the provided ShapeExpression around the y axis (ie, horizontally).
    *
    * @group Transformations
    */
  final case class FlipP(e: ShapeExpression) extends ShapeExpression

  /**
    * Flips the provided ShapeExpression around the x axis (ie, vertically).
    *
    * @group Transformations
    */
  final case class FlipQ(e: ShapeExpression) extends ShapeExpression

  /**
    * @group Transformations
    */
  final case class Translate(e: ShapeExpression, o: Offset) extends ShapeExpression

}
