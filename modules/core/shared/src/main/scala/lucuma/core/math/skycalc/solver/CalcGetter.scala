// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Instant
import gsp.math.Declination
import gsp.math.Angle
import gsp.math.HourAngle
import gsp.math.optics.Wedge
import gsp.math.optics.SplitEpi
import gsp.math.optics.SplitMono
import spire.math.Number
import spire.math.Rational
import java.time.Duration
import monocle.Iso
import gsp.math.optics.Spire._
import io.chrisdavenport.cats.time._

object GetterStrategy {
  sealed trait Closest
  sealed trait LinearInterpolating
}

/**
  * Typeclass defininig how to get a value from a [[Calculator]] using a certain [[GetterStrategy]].
  *
  * @tparam G [[GetterStrategy]] to use
  * @tparam A type of the value to get
  */
trait CalcGetter[G, A] { self =>

  /**
    * Get value from a [[Calculator]].
    *
    * @tparam T the results that the [[Calculator]] holds
    * @param calc [[Calculator]] holding the timed results of type [[T]]
    * @param field how to extract a partial result of type [[A]] from a [[T]]
    * @param instant the desired [[java.time.Instant]] at which to get the value of type [[A]]
    */
  def get(calc: Calculator[A])(instant: Instant): Option[A]

  def imap[B](f: A => B)(g: B => A): CalcGetter[G, B] =
    new CalcGetter[G, B] {
      def get(calc: Calculator[B])(instant: Instant): Option[B] =
        self.get(calc.map(g))(instant).map(f)
    }

  def imap[B](optic: Wedge[A, B]): CalcGetter[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: SplitEpi[A, B]): CalcGetter[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: SplitMono[A, B]): CalcGetter[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: Iso[A, B]): CalcGetter[G, B] =
    imap(optic.get _)(optic.reverseGet)
}

trait CalcGetterInstances {

  implicit def closestGetter[A]: CalcGetter[GetterStrategy.Closest, A] =
    new CalcGetter[GetterStrategy.Closest, A] {
      def get(calc: Calculator[A])(instant: Instant): Option[A] =
        calc.toIndex(instant).map { idx =>
          val timedResult = calc.timedResults(idx)
          if (idx >= calc.instants.length - 1)
            timedResult._2.value
          else {
            val nextTimedResult = calc.timedResults(idx + 1)
            if (
              Duration.between(timedResult._1, instant) <
                Duration.between(instant, nextTimedResult._1)
            )
              timedResult._2.value
            else
              nextTimedResult._2.value
          }
        }
      }

  implicit val interpolatedNumberGetter: CalcGetter[GetterStrategy.LinearInterpolating, Number] =
    new CalcGetter[GetterStrategy.LinearInterpolating, Number] {
      def get(calc: Calculator[Number])(instant: Instant): Option[Number] =
        calc.toIndex(instant).map { idx =>
          val result0 = calc.timedResults(idx)
          val i0      = result0._1
          val v0      = result0._2
          if (i0 === instant || idx === calc.timedResults.length - 1) v0.value
          else {
            val result1 = calc.timedResults(idx + 1)
            val i1      = result1._1
            // require(t0 <= t && t < t1)
            val v1      = result1._2
            val v       =
              v0.value + Rational(instant.toEpochMilli - i0.toEpochMilli,
                            i1.toEpochMilli - i0.toEpochMilli
              ) * (v1.value - v0.value)
            // require((v0 >= v1 && v0 >= v && v >= v1) || (v0 < v1 && v0 <= v && v <= v1))
            v
          }
        }
      }

  // Fails on Infinity
  implicit val interpolatedDoubleGetter: CalcGetter[GetterStrategy.LinearInterpolating, Double] =
    interpolatedNumberGetter.imap(n => numberDouble.get(n.some).get)(d =>
      numberDouble.reverseGet(d.some).get
    )

  // Fails on Infinity
  implicit val interpolatedFloatGetter: CalcGetter[GetterStrategy.LinearInterpolating, Float] =
    interpolatedNumberGetter.imap(n => numberFloat.get(n.some).get)(d =>
      numberFloat.reverseGet(d.some).get
    )

  implicit val interpolatedLongGetter: CalcGetter[GetterStrategy.LinearInterpolating, Long] =
    interpolatedNumberGetter.imap(numberLong)

  implicit val interpolatedIntGetter: CalcGetter[GetterStrategy.LinearInterpolating, Int] =
    interpolatedNumberGetter.imap(numberInt)

  implicit val interpolatedAngleGetter: CalcGetter[GetterStrategy.LinearInterpolating, Angle] =
    interpolatedLongGetter.imap(Angle.microarcseconds.reverse)

  implicit val interpolatedDeclinationGetter
    : CalcGetter[GetterStrategy.LinearInterpolating, Declination] =
    interpolatedAngleGetter.imap((Declination.fromAngleWithCarry _).andThen(_._1))(_.toAngle)

  implicit val interpolatedHourAngleGetter
    : CalcGetter[GetterStrategy.LinearInterpolating, HourAngle] =
    interpolatedAngleGetter.imap(HourAngle.angle.reverse)

  implicit def toTupledGetter[G, A, B](implicit
    getterA: CalcGetter[G, A],
    getterB: CalcGetter[G, B]
  ): CalcGetter[G, (A, B)] =
    new CalcGetter[G, (A, B)] {
      def get(calc: Calculator[(A, B)])(instant: Instant): Option[(A, B)] =
        getterA.get(calc.map(_._1))(instant) product
        getterB.get(calc.map(_._2))(instant)
      }

}

object CalcGetter extends CalcGetterInstances
