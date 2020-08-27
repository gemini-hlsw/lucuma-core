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

/**
  * A set of samples, keyed by `Instant`. Samples are lazy and must be forced by the user, and
  * mapping operations are also applied lazily.
  */
trait Samples[A] { outer =>
  import Samples.{ Bracket, Lookup }

  // We can't use Coyoneda here because our continuation `k` has a richer type, but otherwise the
  // encoding is the same. Values in `data` are of some unknowable type `P` but we know how to map
  // them to `A` and we know what `A` is. So `map` operations can just append to `k` and we don't
  // have to duplicate `data`. Our most general mapping operation is `mapEvalWithKeys` so that
  // determines the type of `k`.
  protected type P // "pivot" type
  protected val data: TreeMap[Instant, Eval[P]]
  protected val k: (Instant, P) => Eval[A]

  /** The value at `i`, if any. */
  def get(i: Instant): Option[Eval[A]] =
    data.get(i).map(_.flatMap(k(i, _)))

  /**
    * Sample data, as a map. Accessing this field has an initial cost of duplicating the underlying
    * structure and applying any mapped fuctions to the value cells (which remain unforced).
    */
  lazy val toMap: TreeMap[Instant, Eval[A]] =
    data.map { case (i, ep) => (i, ep.flatMap(k(i, _))) }

  /**
    * Split the samples at `i`, yielding a `Lookup` containing the matching sample, if any, and all
    * samples before and after `i`.
    */
  def lookup(i: Instant): Lookup[A] = {
    val ls = toMap.rangeUntil(i) // will not contain i
    val rs = toMap.rangeFrom(i)  // may start with i
    if (toMap.contains(i))
      Lookup(ls, rs.headOption, rs.drop(1))
    else
      Lookup(ls, None, rs)
  }

  /**
    * Split the samples at `i`, yielding a `Bracket` containing the matching sample, if any, and the
    * samples immediately before and after `i`, if any.
    */
  def bracket(i: Instant): Bracket[A] = {
    val left  = data.maxBefore(i).map { case (i, ep) => (i, ep.flatMap(k(i, _))) }
    val focus = get(i).map((i, _))
    val right = data.minAfter(i).map { case (i, ep) => (i, ep.flatMap(k(i, _))) }
    Bracket(left, focus, right)
  }

  /** Compute the value at `i`, under a strategy `G`. */
  def valueAt[G](i: Instant)(implicit rounder: SampleRounder[G, A]): Option[A] =
    bracket(i) match {
      // Simple cases, no decisions to make.
      case Bracket(_, Some((_, a)), _)                                 => a.value.some // Exact match
      case Bracket(_, None, None)                                      => none         // Out of bounds or no samples
      case Bracket(None, None, _)                                      => none         // Out of bounds or no samples
      // Use rounder
      case Bracket(Some((leftI, leftE)), None, Some((rightI, rightE))) =>
        rounder.round(leftI, leftE.value, rightI, rightE.value, i)
    }
  // getter.get(this)(i)

  /** Construct a new `Samples` with samples of type `B`. `f` is evaluated lazily. */
  def map[B](f: A => B): Samples[B] =
    mapWithKeys((_, a) => f(a))

  /**
    * Construct a new `Samples` with samples of type `B`, allowing inspection of the associated
    * key. `f` is evaluated lazily.
    */
  def mapWithKeys[B](f: (Instant, A) => B): Samples[B] =
    mapEvalWithKeys((i, a) => Eval.later(f(i, a)))

  /** Construct a new `Samples` with samples of type `B`. */
  def mapEval[B](f: A => Eval[B]): Samples[B] =
    mapEvalWithKeys((_, a) => f(a))

  /**
    * Construct a new `Samples` with samples of type `B`, allowing inspection of the associated
    * key.
    */
  def mapEvalWithKeys[B](f: (Instant, A) => Eval[B]): Samples[B] =
    new Samples[B] {
      type P = outer.P
      val data = outer.data
      val k    = (i, p) => outer.k(i, p).flatMap(a => f(i, a))
    }

  /**
    * Concatenate another `Samples` using `Map` semantics (i.e., in case of conflct the `Samples`
    * on the RHS wins).
    */
  def ++(other: Samples[A]): Samples[A] =
    Samples.fromMap(toMap ++ other.toMap)

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

  /** Construct a `Samples` from a map. */
  def fromMap[A](values: TreeMap[Instant, Eval[A]]): Samples[A] =
    new Samples[A] {
      type P = A
      val data = values
      val k    = (_, a) => Eval.always(a)
    }

  /** Construct a `Samples` with a single sample. */
  def single[A](instant:       Instant, value: => A): Samples[A] =
    fromMap(TreeMap(instant -> Eval.later(value)))

  /** Construct a `Samples` across an interval, sampled at the given rate. */
  def atFixedRate[A](interval: Interval, rate: Duration)(f: Instant => A): Samples[A] =
    fromMap(
      Iterator
        .iterate(interval.start)(_.plus(rate))
        .takeWhile(_ < interval.end.plus(rate))
        .map(i => (i, Eval.later(f(i))))
        .to(TreeMap)
    )

  /** An empty `Samples`. */
  def empty[A]: Samples[A] = ???
  // Samples(TreeMap.empty)

  /** Samples is a covariant functor. */
  implicit val FunctorSamples: Functor[Samples] =
    new Functor[Samples] {
      def map[A, B](fa: Samples[A])(f: A => B) = fa.map(f)
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
      self
        .map { r =>
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
          val weight  = if (airmass <= 1.0) 0.0 else math.pow(airmass - 1.0, 1.3)
          (normalizedAngle * weight, weight)

        }
        .toMap
        .values
        .toList
        .sequence
        .map { list =>
          val (weightedAngles, weights) = list.unzip
          val weightedSum               = weights.sum
          if (weightedSum == 0) None
          else Some(weightedAngles.sum / weightedSum)

        }

  }

}
