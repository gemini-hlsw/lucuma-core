// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Instant
import java.time.Duration
import io.chrisdavenport.cats.time._
import cats.Eval

/**
  * A Calculator holds a list of results which are sampled at defined [[java.time.Instant]]s over a given interval.
  *
  * @tparam A type of the results held
  */
trait Calculator[A] { outer =>

  def instants: List[Instant]

  /** Index of the last instant <= `i`, if any. */
  def toIndex(i: Instant): Option[Int] =
    Some(instants.takeWhile(_ <= i).length - 1).filter(_ >= 0)

  def result(i: Instant): Eval[A]

  lazy val start: Instant   = instants.head
  lazy val end: Instant     = instants.last
  lazy val sampleCount: Int = instants.size

  lazy val results: List[Eval[A]] = instants.map(result)
  lazy val timedResults: List[(Instant, Eval[A])] = instants.zip(results)

  /** True if the values for the given time are covered by this target. */
  def isDefinedAt(i: Instant): Boolean =
    i >= start && i <= end

  def valueAt[G](i: Instant)(implicit getter: CalcGetter[G, A]): Option[A] =
    getter.get(Calculator.this)(i)

  def timedValues: List[(Instant, Eval[A])] =
    timedResults.map { case (i, r) => (i, r) }

  def map[B](f: A => B): Calculator[B] =
    new Calculator[B] {
      def instants: List[Instant] = outer.instants
      def result(i: Instant): Eval[B] = outer.result(i).map(f)
    }

}

/**
  * A Calculator that holds a result for a single [[java.time.Instant]].
  *
  * The caller doesn't need to provide a [[CalcGetter]] when obtaining the result via the <code>value</code> method.
  */
trait SingleValueCalculator[T] extends Calculator[T] {
  val instant: Instant
  val instants = List(instant)
  def value: Option[T] =
    valueAt(instant)(CalcGetter.closestGetter)
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
  override def toIndex(i: Instant): Option[Int] =
    if ((i >= start) && (i <= end))
      Some((Duration.between(start, i).toNanos / rate.toNanos).toInt)
    else
      None

}

/** A Calculator that holds results for the provided [[java.time.Instant]]s */
trait IrregularIntervalCalculator[T] extends Calculator[T] {

  /** Gets the index of the immediately prior existing [[java.time.Instant]]. */
  override def toIndex(i: Instant): Option[Int] =
    if ((instants.size > 0) && (i >= start) && (i <= end)) {
      val ix = instants.zipWithIndex.reverse.dropWhile(_._1 > i).head._2
      // postconditions: useful for debugging / documentation
      // require(ix >= 0 && ix < samples)
      // require(times(ix) <= t && (ix == samples-1 || times(ix+1) > t))
      Some(ix)
    } else None
}
