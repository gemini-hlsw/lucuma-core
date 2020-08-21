// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Instant
import java.time.Duration
import io.chrisdavenport.cats.time._

/**
  * A Calculator holds a list of results which are sampled at defined [[java.time.Instant]]s over a given interval.
  *
  * @tparam T type of the results held
  */
trait Calculator[T] {
  def instants: List[Instant]
  def toIndex(i: Instant): Int // Must return the first Instant >= i
  def result: Instant => T

  lazy val start: Instant   = instants.head
  lazy val end: Instant     = instants.last
  lazy val sampleCount: Int = instants.size

  lazy val results: List[T]                 = instants.map(result)
  lazy val timedResults: List[(Instant, T)] = instants.zip(results)

  /** True if the values for the given time are covered by this target. */
  def isDefinedAt(i: Instant): Boolean =
    i >= start && i <= end

  def result(ix: Int): T = results(ix)

  def valueAt[G]: GetterWithStrategy[G] = new GetterWithStrategy[G]

  protected class GetterWithStrategy[G] {
    def apply[A](field: T => A)(i: Instant)(implicit getter: CalcGetter[G, A]): A =
      getter.get(Calculator.this)(field)(i)
  }

  def timedValues[A](field:   T => A): List[(Instant, A)] =
    timedResults.map { case (i, r) => (i, field(r)) }

  def min[A: Ordering](field: T => A): A                  = field(results.minBy(field))
  def max[A: Ordering](field: T => A): A                  = field(results.maxBy(field))
  def mean(field:             T => Double): Double        = results.map(field).sum / sampleCount
}

/**
  * A Calculator that holds a result for a single [[java.time.Instant]].
  *
  * The caller doesn't need to provide a [[CalcGetter]] when obtaining the result via the <code>value</code> method.
  */
trait SingleValueCalculator[T] extends Calculator[T] {
  val instant: Instant
  val instants = List(instant)
  def toIndex(i: Instant) = 0

  def value[A](field: T => A): A =
    valueAt(field)(instant)(CalcGetter.closestGetter)
}

/** A Calculator that holds results for evenly spaced [[java.time.Instant]]s within an [[Interval]] */
trait FixedRateCalculator[T] extends Calculator[T] {
  def interval: Interval
  def rate: Duration

  private lazy val maxEnd: Instant = interval.end.plus(rate)

  /** Calculates a list with instants that cover the given interval at the specified rate. */
  override lazy val instants: List[Instant] = {
    require(rate > Duration.ZERO)
    List.unfold(interval.start) {
      case i if i < maxEnd => (i, i.plus(rate)).some
      case _               => none
    }
  }

  /** Gets the index of the immediately prior existing [[java.time.Instant]]. */
  override def toIndex(i: Instant): Int = {
    require(i >= start)
    require(i <= end)
    (Duration.between(start, i).toNanos / rate.toNanos).toInt
  }
}

/** A Calculator that holds results for the provided [[java.time.Instant]]s */
trait IrregularIntervalCalculator[T] extends Calculator[T] {

  /** Gets the index of the immediately prior existing [[java.time.Instant]]. */
  override def toIndex(i: Instant): Int = {
    require(instants.size > 0)
    require(i >= start)
    require(i <= end)
    val ix = instants.zipWithIndex.reverse.dropWhile(_._1 > i).head._2
    // postconditions: useful for debugging / documentation
    // require(ix >= 0 && ix < samples)
    // require(times(ix) <= t && (ix == samples-1 || times(ix+1) > t))
    ix
  }
}
