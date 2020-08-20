// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Instant
import gsp.math.Declination
import gsp.math.Angle
import gsp.math.HourAngle
import io.chrisdavenport.cats.time._

object implicits {

  implicit def exactGetter[A]: CalcGetter[GetterStrategy.Exact, A] =
    new CalcGetter[GetterStrategy.Exact, A] {
      def get[T](calc: Calculator[GetterStrategy.Exact, T])(field: T => A)(instant: Instant): A =
        field(calc.timedResults(calc.toIndex(instant))._2)
    }

  implicit val interpolatedDoubleGetter: CalcGetter[GetterStrategy.LinearInterpolating, Double] =
    new CalcGetter[GetterStrategy.LinearInterpolating, Double] {
      def get[T](
        calc:  Calculator[GetterStrategy.LinearInterpolating, T]
      )(field: T => Double)(instant: Instant): Double = {
        val idx     = calc.toIndex(instant)
        val result0 = calc.timedResults(idx)
        val i0      = result0._1
        val v0      = field(result0._2)
        if (i0 === instant || idx === calc.timedResults.length - 1) v0
        else {
          val result1 = calc.timedResults(idx + 1)
          val i1      = result1._1
          // require(t0 <= t && t < t1)
          val v1      = field(result1._2)
          val v       =
            v0 + (instant.toEpochMilli - i0.toEpochMilli).toDouble / (i1.toEpochMilli - i0.toEpochMilli) * (v1 - v0)
          // require((v0 >= v1 && v0 >= v && v >= v1) || (v0 < v1 && v0 <= v && v <= v1))
          v
        }
      }
    }

  implicit val interpolatedDeclinationGetter
    : CalcGetter[GetterStrategy.LinearInterpolating, Declination] =
    new CalcGetter[GetterStrategy.LinearInterpolating, Declination] {
      def get[T](
        calc:  Calculator[GetterStrategy.LinearInterpolating, T]
      )(field: T => Declination)(instant: Instant): Declination =
        Declination
          .fromAngleWithCarry(
            Angle.fromDoubleDegrees(
              interpolatedDoubleGetter
                .get(calc)(field.andThen(_.toAngle.toSignedDoubleDegrees))(instant)
            )
          )
          ._1
    }

  implicit val interpolatedHourAngleGetter
    : CalcGetter[GetterStrategy.LinearInterpolating, HourAngle] =
    new CalcGetter[GetterStrategy.LinearInterpolating, HourAngle] {
      def get[T](
        calc:  Calculator[GetterStrategy.LinearInterpolating, T]
      )(field: T => HourAngle)(instant: Instant): HourAngle =
        HourAngle.fromDoubleDegrees(
          interpolatedDoubleGetter.get(calc)(field.andThen(_.toDoubleDegrees))(instant)
        )
    }

  implicit def toTupledGetter[G, A, B](implicit
    getterA: CalcGetter[G, A],
    getterB: CalcGetter[G, B]
  ): CalcGetter[G, (A, B)] =
    new CalcGetter[G, (A, B)] {
      def get[T](calc: Calculator[G, T])(field: T => (A, B))(instant: Instant): (A, B) =
        (
          getterA.get(calc)(field.andThen(_._1))(instant),
          getterB.get(calc)(field.andThen(_._2))(instant)
        )
    }
}
