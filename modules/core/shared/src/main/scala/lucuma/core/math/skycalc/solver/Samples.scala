// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc
package solver

import cats.{ Eval, Functor, MonoidK }
import cats.implicits._
import gsp.math.{ Coordinates, Place }
import io.chrisdavenport.cats.time._
import java.time.{ Duration, Instant }
import scala.collection.immutable.TreeMap

/** A set of samples, keyed by `Instant`. Samples are lazy and must be forced by the user. */
final case class Samples[A](data: TreeMap[Instant, Eval[A]]) {
  import Samples.{ Bracket, Lookup }

  /**
   * Split the samples at `i`, yielding a `Lookup` containing the matching sample, if any, and all
   * samples before and after `i`.
   */
  def lookup(i: Instant): Samples.Lookup[A] = {
    val ls = data.rangeUntil(i) // will not contain i
    val rs = data.rangeFrom(i)  // may start with i
    if (data.contains(i)) {
      Samples.Lookup(ls, rs.headOption, rs.drop(1))
    } else {
      Samples.Lookup(ls, None, rs)
    }
  }

  /**
   * Split the samples at `i`, yielding a `Lookup` containing the matching sample, if any, and the
   * samples immediately before and after `i`, if any.
   */
  def bracket(i: Instant): Bracket[A] = {
    // TODO: we can compute this more efficiently
    val Lookup(ls, f, rs) = lookup(i)
    Bracket(ls.lastOption, f, rs.headOption)
  }

  /** Compute the value at `i`, under a strategy `G`. */
  def valueAt[G](i: Instant)(implicit getter: CalcGetter[G, A]): Option[A] =
    getter.get(this)(i)

  /** Construct a new `Samples` with samples of type `B`. */
  def map[B](f: A => B): Samples[B] =
    Samples(data.map { case (k, v) => (k, v map f)})

  /**
   * Construct a new `Samples` with samples of type `B`, allowing inspection of the associated
   * key.
   */
  def mapWithKeys[B](f: (Instant, A) => B): Samples[B] =
    Samples(data.map { case (k, v) => (k, v.map(f(k, _))) })

  /**
   * Concatenate another `Samples` using `Map` semantics (i.e., in case of conflct the `Samples`
   * on the RHS wins).
   */
  def ++(other: Samples[A]): Samples[A] =
    Samples(data ++ other.data)

}

object Samples {

  /**
   * A lookup result, which contains the samples prior to `i`, the sample at `i` (if any), and the
   * samples subsequent to `i`.
   */
  case class Lookup[A](
    lefts:  TreeMap[Instant, Eval[A]],
    focus:  Option[(Instant, Eval[A])],
    rights: TreeMap[Instant, Eval[A]]
  )

  /**
   * A bracket result, which contains the sample immediately prior to `i` (if any), the sample at
   * `i` (if any), and the sample immediately following to `i` (if any).
   */
  case class Bracket[A](
    left:  Option[(Instant, Eval[A])],
    focus: Option[(Instant, Eval[A])],
    right: Option[(Instant, Eval[A])]
  )

  /** Construct a `Samples` with a single sample. */
  def single[A](instant: Instant, value: => A): Samples[A] =
    Samples(TreeMap(instant -> Eval.later(value)))

  /** Construct a `Samples` across an interval, sampled at the given rate. */
  def atFixedRate[A](interval: Interval, rate: Duration)(f: Instant => A): Samples[A] = {
    Samples(
      Iterator
        .iterate(interval.start)(_.plus(rate))
        .takeWhile(_ < interval.end.plus(rate))
        .map(i => (i, Eval.later(f(i))))
        .to(TreeMap)
    )
  }

  /** An empty `Samples`. */
  def empty[A]: Samples[A] =
    Samples(TreeMap.empty)

  /** Samples is a covariant functor. */
  implicit val FunctorSamples: Functor[Samples] =
    new Functor[Samples] {
      def map[A,B](fa: Samples[A])(f: A => B) = fa.map(f)
    }

  /** `Samples` is a `MonoidK`. */
  implicit val MonoidKSamples: MonoidK[Samples] =
    new MonoidK[Samples] {
      def combineK[A](x: Samples[A], y: Samples[A]) = x ++ y
      def empty[A] = Samples.empty
    }

  /** Convenience syntax for sampled `Coordinates`. */
  implicit class CoordinateSamplesSyntax(self: Samples[Coordinates]) {

    /** Compute skycalc results at each instant. */
    def toSkyCalResultsAt(place: Place): Samples[SkyCalcResults] = {
      val skycalc = ImprovedSkyCalc(place)
      self.mapWithKeys { case (i, cs) => skycalc.calculate(cs, i, true) }
    }

  }

  /** Convenience syntax for sampled `SkyCalcResults`. */
  implicit class SkyCalcResultsSamplesSyntax(self: Samples[SkyCalcResults]) {

    /**
     * Compute the weighted mean parallactic angle over this `Samples`, if the target is visible for
     * at least one sample.
     */
    lazy val weightedMeanParallacticAngle: Eval[Option[Double]] =
      self.map { r =>

        val angle   = r.parallacticAngleRaw
        val airmass = r.airmass
        val normalizedAngle = {
          if (angle < 0) {
            val normalizingFactor = {
              val dec = r.coordinates.dec.toAngle.toSignedDoubleDegrees
              if (dec - r.place.latitude.toAngle.toSignedDoubleDegrees < -10) 0
              else if (dec - r.place.latitude.toAngle.toSignedDoubleDegrees < 10) 180
              else 360
            }
            angle + normalizingFactor
          } else angle
        }
        val weight = if (airmass <= 1.0) 0.0 else math.pow(airmass - 1.0, 1.3)
        (normalizedAngle * weight, weight)

      }
      .data
      .values
      .toList
      .sequence
      .map { list =>

        val (weightedAngles, weights) = list.unzip
        val weightedSum = weights.sum
        if (weightedSum == 0) None
        else Some(weightedAngles.sum / weightedSum)

      }

    }

}


