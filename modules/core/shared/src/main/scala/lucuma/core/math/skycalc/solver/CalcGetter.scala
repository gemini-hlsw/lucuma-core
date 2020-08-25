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
import monocle.Iso
import gsp.math.optics.Spire._
import gsp.math.skycalc.solver.Samples.Bracket
import cats.Eval
import scala.collection.immutable.Nil

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
  def get(calc: Samples[A])(instant: Instant): Option[A]

  def imap[B](f: A => B)(g: B => A): CalcGetter[G, B] =
    new CalcGetter[G, B] {
      def get(calc: Samples[B])(instant: Instant): Option[B] =
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
      def get(calc: Samples[A])(instant: Instant): Option[A] =
        calc.bracket(instant) match {

          // Simple cases, no decisions to make.
          case Bracket(_, Some((_, a)), _)       => a.value.some // Exact match
          case Bracket(Some((_, a)), None, None) => a.value.some // All are smaller, last one wins
          case Bracket(None, None, Some((_, a))) => a.value.some // All are larger, first one wins
          case Bracket(None, None, None)         => none         // No samples at all, no match.

          // In range but not an exact match. Pick the closer one.
          case Bracket(Some((i0, a)), None, Some((i1, b))) => a.value.some
            val d0 = instant.toEpochMilli - i0.toEpochMilli
            val d1 = i1.toEpochMilli - instant.toEpochMilli
            (if (d0 < d1) a else b).value.some

        }
      }

  implicit val interpolatedNumberGetter: CalcGetter[GetterStrategy.LinearInterpolating, Number] =
    new CalcGetter[GetterStrategy.LinearInterpolating, Number] {
      def get(calc: Samples[Number])(instant: Instant): Option[Number] = {

        def interp(
          e0: (Instant, Eval[Number]),
          e1: (Instant, Eval[Number])
        ): Eval[Number] =
          for {
            v0 <- e0._2
            i0  = e0._1
            v1 <- e1._2
            i1  = e1._1
          } yield {
              v0 + Rational(
                instant.toEpochMilli - i0.toEpochMilli,
                i1.toEpochMilli      - i0.toEpochMilli
              ) * (v1 - v0)
          }

        calc.bracket(instant) match {
          case Bracket(_, Some((_, n)), _) => n.value.some // exact match
          case Bracket(left, None, right)  =>
            if (left.nonEmpty && right.nonEmpty) {
              // There's at least one point on the left and one on the right
              interp(left.last, right.head).value.some
            } else {
              // One side is empty, so take two from the end of the left and front of the right, and
              // we'll have at most two points.
              (left.takeRight(2).toList ++ right.take(2).toList) match {
                case e0 :: e1 :: Nil => interp(e0, e1).value.some // interpolate
                case (_, n0)  :: Nil => n0.value.some             // assume constant if there's only one entry
                case _               => none                      // no points, can't even guess!
              }
            }
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
      def get(calc: Samples[(A, B)])(instant: Instant): Option[(A, B)] =
        getterA.get(calc.map(_._1))(instant) product
        getterB.get(calc.map(_._2))(instant)
      }

}

object CalcGetter extends CalcGetterInstances
