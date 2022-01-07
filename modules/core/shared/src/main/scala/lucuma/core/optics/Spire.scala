// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.Order
import cats.syntax.all._
import lucuma.core.optics.Format
import lucuma.core.optics.SplitEpi
import lucuma.core.syntax.boundedInterval._
import monocle.Iso
import spire.math._
import spire.math.extras.interval.IntervalSeq

import scala.util.Try

object Spire {
  val numberInt: SplitEpi[Number, Int] = SplitEpi(_.intValue, Number.apply)

  val numberLong: SplitEpi[Number, Long] = SplitEpi(_.longValue, Number.apply)

  // Deal with Infinity and overflows
  val numberFloat: SplitEpi[Option[Number], Option[Float]] =
    SplitEpi(
      _.map(_.floatValue).filterNot(f => java.lang.Float.isNaN(f) || java.lang.Float.isInfinite(f)),
      _.flatMap(f => Try(Number(f)).toOption)
    )

  // Deal with Infinity and overflows
  val numberDouble: SplitEpi[Option[Number], Option[Double]] =
    SplitEpi(
      _.map(_.doubleValue).filterNot(f =>
        java.lang.Double.isNaN(f) || java.lang.Double.isInfinite(f)
      ),
      _.flatMap(f => Try(Number(f)).toOption)
    )

  val numberBigInt: SplitEpi[Number, BigInt] = SplitEpi(_.toBigInt, Number.apply)

  val numberSafeLong: SplitEpi[Number, SafeLong] = SplitEpi(n => SafeLong(n.toBigInt), Number.apply)

  val numberBigDecimal: Iso[Number, BigDecimal] = Iso((n: Number) => n.toBigDecimal)(Number.apply)

  // Number <-> Rational is actually an Iso in the JVM and a SplitEpi in JS, because of Long precision.
  // val numberRational: SplitEpi[Number, Rational] = SplitEpi(_.toRational, Number.apply)
  // val numberRational: Iso[Number, Rational]      = Iso((n: Number) => n.toRational)(Number.apply)

  val numberNatural: Format[Number, Natural] =
    Format(n => Try(Natural(n.toBigInt)).toOption, Number.apply)

  /**
   * Makes a best-effort attempt to convert the tuple (a, b) into interval [a, b) or [b, a).
   *
   * It's a Format to contemplate the case of tuple (a, a).
   */
  def openUpperIntervalFromTuple[A: Order]: Format[(A, A), Bounded[A]] =
    Format[(A, A), Bounded[A]](
      { case (start, end) =>
        if (start < end)(Bounded.unsafeOpenUpper(start, end)).some
        else if (start > end)(Bounded.unsafeOpenUpper(end, start)).some
        else none
      },
      i => (i.lower, i.upper)
    )

  /**
   * Makes a best-effort attempt to convert the tuple (a, b) into interval [a, b] or [b, a].
   *
   * It's a Format to contemplate the case of tuple (a, a).
   */
  def closedIntervalFromTuple[A: Order]: Format[(A, A), Bounded[A]] =
    Format[(A, A), Bounded[A]](
      { case (start, end) =>
        if (start < end)(Bounded.unsafeClosed(start, end)).some
        else if (start > end)(Bounded.unsafeClosed(end, start)).some
        else none
      },
      i => (i.lower, i.upper)
    )

  /**
   * The union of any list of `Interval[A]`.
   *
   * Building an InervalSeq from a List will normalize it into a minimal set covering the Intervals.
   */
  def intervalListUnion[A: Order]: SplitEpi[List[Interval[A]], IntervalSeq[A]] =
    SplitEpi[List[Interval[A]], IntervalSeq[A]](
      _.foldLeft(IntervalSeq.empty[A])((s, i) => s | i),
      _.intervals.toList
    )
}
