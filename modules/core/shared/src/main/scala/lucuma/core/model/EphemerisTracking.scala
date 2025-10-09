// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Foldable
import cats.Monoid
import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.core.optics.SplitMono
import lucuma.core.syntax.treemap.*
import lucuma.core.util.Timestamp

import scala.collection.immutable.TreeMap

/**
 * Time-parameterized coordinates over a fixed interval, defined pairwise. Coordinates that fall
 * between known instants are interpolated.
 */
sealed abstract case class EphemerisTracking private (toMap: TreeMap[Timestamp, EphemerisCoordinates]) {
  import EphemerisTracking.Element

  // N.B. this case class is abstract and has a private ctor because we want to keep construction of
  // the TreeMap private to ensure that the correct ordering is used and remains consistent.

  /** First element in this ephemeris, if any. */
  def first: Option[Element] =
    toMap.headOption

  /** Last element in this ephemeris, if any. */
  def last: Option[Element] =
    toMap.lastOption

  /** Coordinates at time `t`, exact if known, interpolated if `bracket(t)` is known. */
  def get(t: Timestamp): Option[EphemerisCoordinates] =
    toMap
      .get(t)
      .orElse(bracket(t).map {
        case ((a, ca), (b, cb)) =>
          val (iʹ, aʹ, bʹ) = (t.toEpochMilli, a.toEpochMilli, b.toEpochMilli)
          val factor       = (iʹ - aʹ).toDouble / (bʹ - aʹ).toDouble
          ca.interpolate(cb, factor)
      })

  /**
   * Greatest lower and least upper bounds of `t`; i.e., the closest elements on either side,
   * inclusive (so if `t` is present then `bracket(t) = (t, t)`).
   */
  def bracket(t: Timestamp): Option[(Element, Element)] =
    (toMap.rangeTo(t).lastOption, toMap.rangeFrom(t).headOption).tupled

  /** The sum of this ephemeris and `e`, taking values from `e` in the case of overlap. */
  def ++(e: EphemerisTracking): EphemerisTracking =
    new EphemerisTracking(toMap ++ e.toMap) {}

}
object EphemerisTracking {

  /** An ephemeris element. */
  type Element = (Timestamp, EphemerisCoordinates)

  /** The empty ephemeris. */
  val empty: EphemerisTracking = apply()

  /** Construct an ephemeris from a sequence of literal elements. */
  def apply(es: Element*): EphemerisTracking =
    fromList(es.toList)

  /** Construct an ephemeris from a `List` of elements. */
  def fromList(es:                     List[Element]): EphemerisTracking =
    new EphemerisTracking(TreeMap.fromList(es)) {}

  /** Construct an ephemeris from a foldable of elements. */
  def fromFoldable[F[_]: Foldable](fa: F[Element]): EphemerisTracking    =
    fromList(fa.toList)

  /** Ephemerides form a monoid, using `++` as the combining operation. */
  given Monoid[EphemerisTracking] =
    new Monoid[EphemerisTracking] {
      val empty: EphemerisTracking = EphemerisTracking.empty
      def combine(a: EphemerisTracking, b: EphemerisTracking) = a ++ b
    }

  given Eq[EphemerisTracking] =
    Eq.fromUniversalEquals

  val elements: SplitMono[EphemerisTracking, List[Element]] =
    SplitMono(_.toMap.toList, fromFoldable(_))

}
