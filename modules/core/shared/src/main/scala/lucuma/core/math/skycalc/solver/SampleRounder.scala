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
import cats.InvariantSemigroupal

object RoundStrategy {
  sealed trait Closest
  sealed trait LinearInterpolating
}

/**
  * Typeclass defininig how to round values associated with `Instant`s using a certain [[RoundStrategy]].
  *
  * @tparam R [[RoundStrategy]] to use
  * @tparam A type of the value
  */
trait SampleRounder[R, A] { self =>

  /**
    * Round values associated with `Instant`s.
    */
  def round(leftI: Instant, leftV: A, rightI: Instant, rightV: A, i: Instant): Option[A]

  // weird signature, keep private
  private def contraMapRound[B](
    f:     B => A
  )(leftI: Instant, leftV: B, rightI: Instant, rightV: B, i: Instant): Option[A] =
    round(leftI, f(leftV), rightI, f(rightV), i)

  // weird signature, keep private
  private def contraMapRoundOpt[B](
    f:     B => Option[A]
  )(leftI: Instant, leftV: B, rightI: Instant, rightV: B, i: Instant): Option[A] =
    (for {
      leftA  <- f(leftV)
      rightA <- f(rightV)
    } yield round(leftI, leftA, rightI, rightA, i)).flatten

  def product[B](rounderB: SampleRounder[R, B]): SampleRounder[R, (A, B)] =
    new SampleRounder[R, (A, B)] {
      def round(leftI: Instant, leftV: (A, B), rightI: Instant, rightV: (A, B), i: Instant): Option[(A, B)] =
        self
          .contraMapRound[(A, B)](_._1)(leftI, leftV, rightI, rightV, i)
          .product(rounderB.contraMapRound[(A, B)](_._2)(leftI, leftV, rightI, rightV, i))
    }

  def imap[B](f: A => B)(g: B => A): SampleRounder[R, B] =
    new SampleRounder[R, B] {
      def round(leftI: Instant, leftV: B, rightI: Instant, rightV: B, i: Instant): Option[B] =
        self.contraMapRound(g)(leftI, leftV, rightI, rightV, i).map(f)
    }

  def imap[B](optic: Wedge[A, B]): SampleRounder[R, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: SplitEpi[A, B]): SampleRounder[R, B] =
    imap(optic.asWedge)

  def imap[B](optic: SplitMono[A, B]): SampleRounder[R, B] =
    imap(optic.asWedge)

  def imap[B](optic: Iso[A, B]): SampleRounder[R, B] =
    imap(Wedge.fromIso(optic))

  def imapOpt[B](f: A => Option[B])(g: B => Option[A]): SampleRounder[R, B] =
    new SampleRounder[R, B] {
      def round(leftI: Instant, leftV: B, rightI: Instant, rightV: B, i: Instant): Option[B] =
        self.contraMapRoundOpt(g)(leftI, leftV, rightI, rightV, i).flatMap(f)
    }

  def imapOpt[B](optic: Wedge[Option[A], Option[B]]): SampleRounder[R, B] =
    imapOpt(a => optic.get(a.some))(b => optic.reverseGet(b.some))

  def imapOpt[B](optic: SplitMono[Option[A], Option[B]]): SampleRounder[R, B] =
    imapOpt(optic.asWedge)

  def imapOpt[B](optic: SplitEpi[Option[A], Option[B]]): SampleRounder[R, B] =
    imapOpt(optic.asWedge)

  def imapOpt[B](optic: Iso[Option[A], Option[B]]): SampleRounder[R, B] =
    imapOpt(Wedge.fromIso(optic))
}

trait SampleRounderInstances {
  import RoundStrategy._

  /** SampleRounder is an invariant semigroupal functor. */
  implicit def invariantSemigroupalSampleRounder[R]: InvariantSemigroupal[SampleRounder[R, *]] =
    new InvariantSemigroupal[SampleRounder[R, *]] {
      def product[A, B](fa: SampleRounder[R,A], fb: SampleRounder[R,B]) = fa.product(fb)
      def imap[A, B](fa: SampleRounder[R,A])(f: A => B)(g: B => A) = fa.imap(f)(g)
    }

  implicit def closestRounder[A]: SampleRounder[Closest, A] =
    new SampleRounder[Closest, A] {
      def round(leftI: Instant, leftV: A, rightI: Instant, rightV: A, i: Instant): Option[A] = {
        val leftD  = Duration.between(leftI, i)
        val rightD = Duration.between(i, rightI)
        (if (leftD < rightD) leftV else rightV).some
      }
    }

  implicit val interpolatedNumberRounder: SampleRounder[LinearInterpolating, Number] =
    new SampleRounder[LinearInterpolating, Number] {
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

  // Returns None if any value is Infinity
  implicit val interpolatedDoubleRounder: SampleRounder[LinearInterpolating, Double] =
    interpolatedNumberRounder.imapOpt(numberDouble)

  // Returns None if any value is Infinity
  implicit val interpolatedFloatRounder: SampleRounder[LinearInterpolating, Float] =
    interpolatedNumberRounder.imapOpt(numberFloat)

  implicit val interpolatedLongRounder: SampleRounder[LinearInterpolating, Long] =
    interpolatedNumberRounder.imap(numberLong)

  implicit val interpolatedIntRounder: SampleRounder[LinearInterpolating, Int] =
    interpolatedNumberRounder.imap(numberInt)

  implicit val interpolatedAngleRounder: SampleRounder[LinearInterpolating, Angle] =
    interpolatedLongRounder.imap(Angle.microarcseconds.reverse)

  implicit val interpolatedDeclinationRounder: SampleRounder[LinearInterpolating, Declination] =
    interpolatedAngleRounder.imap((Declination.fromAngleWithCarry _).andThen(_._1))(_.toAngle)

  implicit val interpolatedHourAngleRounder: SampleRounder[LinearInterpolating, HourAngle] =
    interpolatedAngleRounder.imap(HourAngle.angle.reverse)

  implicit def toTupledRounder[R, A, B](implicit
    rounderA: SampleRounder[R, A],
    rounderB: SampleRounder[R, B]
  ): SampleRounder[R, (A, B)] =
    rounderA product rounderB

}

object SampleRounder extends SampleRounderInstances
