// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Duration
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
import io.chrisdavenport.cats.time._

object RoundStrategy {
  sealed trait Closest
  sealed trait LinearInterpolating
}

/**
  * Typeclass defininig how to round values from [[Samples]] using a certain [[RoundStrategy]].
  *
  * @tparam G [[RoundStrategy]] to use
  * @tparam A type of the value in [[Samples]]
  */
trait SampleRounder[G, A] { self =>

  /**
    * Compute rounder value from [[Samples]] when there's not an exact match.
    */
  def round(leftI: Instant, leftV: A, rightI: Instant, rightV: A, i: Instant): Option[A]

  def contraMapRound[B](
    f:     B => A
  )(leftI: Instant, leftV: B, rightI: Instant, rightV: B, i: Instant): Option[A] =
    round(leftI, f(leftV), rightI, f(rightV), i)

  // def contraFlatMapRound[B](f: B => Option[A])(leftI: Instant, leftV: B, rightI: Instant, rightV: B, i: Instant): Option[A] =
  //   f(leftV).fl

  //   round(sampleMap(left), sampleMap(right))(i)
  // }

  def imap[B](f: A => B)(g: B => A): SampleRounder[G, B] =
    new SampleRounder[G, B] {
      def round(leftI: Instant, leftV: B, rightI: Instant, rightV: B, i: Instant): Option[B] =
        self.contraMapRound(g)(leftI, leftV, rightI, rightV, i).map(f)
    }

  def imap[B](optic: Wedge[A, B]): SampleRounder[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: SplitEpi[A, B]): SampleRounder[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: SplitMono[A, B]): SampleRounder[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: Iso[A, B]): SampleRounder[G, B] =
    imap(optic.get _)(optic.reverseGet)

  // def forOption: SampleRounder[G, Option[A]] =
  //   new SampleRounder[G, Option[A]] {
  //     override def get(samples: Samples[Option[A]])(instant: Instant): Option[Option[A]] = ???
  //   }

  // def imapOpt[B](f: A => Option[B])(g: B => Option[A]): SampleRounder[G, B] =
  //   new SampleRounder[G, B] {
  //     def get(samples: Samples[B])(instant: Instant): Option[B] =
  //       self.get(samples.map[Option[A]](g))(instant).map(f).flatten
  //   }
}

trait SampleRounderInstances {

  implicit def closestRounder[A]: SampleRounder[RoundStrategy.Closest, A] =
    new SampleRounder[RoundStrategy.Closest, A] {
      def round(leftI: Instant, leftV: A, rightI: Instant, rightV: A, i: Instant): Option[A] = {
        val leftD  = Duration.between(leftI, i)
        val rightD = Duration.between(i, rightI)
        (if (leftD < rightD) leftV else rightV).some
      }
    }

  implicit val interpolatedNumberRounder: SampleRounder[RoundStrategy.LinearInterpolating, Number] =
    new SampleRounder[RoundStrategy.LinearInterpolating, Number] {
      def round(
        leftI:  Instant,
        leftV:  Number,
        rightI: Instant,
        rightV: Number,
        i:      Instant
      ): Option[Number] =
        (leftV + Rational(
          Duration.between(leftI, i).toNanos,
          Duration.between(leftI, rightI).toNanos
        ) * (rightV - leftV)).some
    }

  // Fails on Infinity
  implicit val interpolatedDoubleRounder: SampleRounder[RoundStrategy.LinearInterpolating, Double] =
    // def round(left: (Instant, Eval[Double]), right: (Instant, Eval[Double]))(
    // i:            Instant
    // ): Option[Double] =
    interpolatedNumberRounder.imap(n => numberDouble.get(n.some).get)(d =>
      numberDouble.reverseGet(d.some).get
    )

  // Fails on Infinity
  implicit val interpolatedFloatRounder: SampleRounder[RoundStrategy.LinearInterpolating, Float] =
    interpolatedNumberRounder.imap(n => numberFloat.get(n.some).get)(d =>
      numberFloat.reverseGet(d.some).get
    )

  implicit val interpolatedLongRounder: SampleRounder[RoundStrategy.LinearInterpolating, Long] =
    interpolatedNumberRounder.imap(numberLong)

  implicit val interpolatedIntRounder: SampleRounder[RoundStrategy.LinearInterpolating, Int] =
    interpolatedNumberRounder.imap(numberInt)

  implicit val interpolatedAngleRounder: SampleRounder[RoundStrategy.LinearInterpolating, Angle] =
    interpolatedLongRounder.imap(Angle.microarcseconds.reverse)

  implicit val interpolatedDeclinationRounder
    : SampleRounder[RoundStrategy.LinearInterpolating, Declination] =
    interpolatedAngleRounder.imap((Declination.fromAngleWithCarry _).andThen(_._1))(_.toAngle)

  implicit val interpolatedHourAngleRounder
    : SampleRounder[RoundStrategy.LinearInterpolating, HourAngle] =
    interpolatedAngleRounder.imap(HourAngle.angle.reverse)

  implicit def toTupledRounder[G, A, B](implicit
    rounderA: SampleRounder[G, A],
    rounderB: SampleRounder[G, B]
  ): SampleRounder[G, (A, B)] =
    new SampleRounder[G, (A, B)] {
      def round(
        leftI:  Instant,
        leftV:  (A, B),
        rightI: Instant,
        rightV: (A, B),
        i:      Instant
      ): Option[(A, B)] =
        rounderA
          .contraMapRound[(A, B)](_._1)(leftI, leftV, rightI, rightV, i)
          .product(rounderB.contraMapRound[(A, B)](_._2)(leftI, leftV, rightI, rightV, i))
    }

}

object SampleRounder extends SampleRounderInstances
