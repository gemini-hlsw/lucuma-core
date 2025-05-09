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
sealed abstract case class Ephemeris private (toMap: TreeMap[Timestamp, EphemerisCoordinates]) {
  import Ephemeris.Element

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
  def ++(e: Ephemeris): Ephemeris =
    new Ephemeris(toMap ++ e.toMap) {}

}
object Ephemeris {

  /** An ephemeris element. */
  type Element = (Timestamp, EphemerisCoordinates)

  /** The empty ephemeris. */
  val empty: Ephemeris = apply()

  /** Construct an ephemeris from a sequence of literal elements. */
  def apply(es: Element*): Ephemeris =
    fromList(es.toList)

  /** Construct an ephemeris from a `List` of elements. */
  def fromList(es:                     List[Element]): Ephemeris =
    new Ephemeris(TreeMap.fromList(es)) {}

  /** Construct an ephemeris from a foldable of elements. */
  def fromFoldable[F[_]: Foldable](fa: F[Element]): Ephemeris    =
    fromList(fa.toList)

  /** Ephemerides form a monoid, using `++` as the combining operation. */
  given Monoid[Ephemeris] =
    new Monoid[Ephemeris] {
      val empty: Ephemeris = Ephemeris.empty
      def combine(a: Ephemeris, b: Ephemeris) = a ++ b
    }

  given Eq[Ephemeris] =
    Eq.fromUniversalEquals

  val elements: SplitMono[Ephemeris, List[Element]] =
    SplitMono(_.toMap.toList, fromFoldable(_))

}
