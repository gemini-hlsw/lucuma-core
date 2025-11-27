// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.geom._
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 4, time = 1)
@Measurement(iterations = 10, time = 10)
class ShapeExpressionBenchmark:

  val rect: ShapeExpression =
    ShapeExpression.centeredRectangle(100.arcseconds, 50.arcseconds)

  val composite: ShapeExpression = {
    val rect1   = ShapeExpression.centeredRectangle(100.arcseconds, 100.arcseconds)
    val rect2   = ShapeExpression.centeredRectangle(80.arcseconds, 80.arcseconds) ↗
      Offset.signedDecimalArcseconds.reverseGet(10.0, 10.0)
    val ellipse = ShapeExpression.centeredEllipse(60.arcseconds, 40.arcseconds)
    (rect1 ∩ rect2) ∪ ellipse
  }

  val testOffset: Offset = Offset.signedDecimalArcseconds.reverseGet(25.0, 25.0)

  val evaluatedShape1: Shape = rect.eval
  val evaluatedShape2: Shape = composite.eval

  val rotatedExpr: ShapeExpression = rect ⟲ Angle.fromDoubleDegrees(45.0)

  @Benchmark
  def evalSimpleShape: Shape =
    rect.eval

  @Benchmark
  def evalComplexExpression: Shape =
    composite.eval

  @Benchmark
  def evalRotatedShape: Shape =
    rotatedExpr.eval

  @Benchmark
  def shapeContains: Boolean =
    evaluatedShape1.contains(testOffset)

  @Benchmark
  def shapeIntersection: Shape =
    evaluatedShape1.intersection(evaluatedShape2)

  @Benchmark
  def shapeIntersects: Boolean =
    evaluatedShape1.intersects(evaluatedShape2)

  @Benchmark
  def shapeArea: Area =
    evaluatedShape2.area

  @Benchmark
  def shapeBoundingOffsets: BoundingOffsets =
    evaluatedShape2.boundingOffsets
