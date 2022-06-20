// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GPI Filter.
 * @group Enumerations
 */
sealed abstract class GpiFilter(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val band:      Band,
  val obsolete:  Boolean
) extends Product
    with Serializable

object GpiFilter {

  /** @group Constructors */
  case object Y extends GpiFilter("Y", "Y", "Y", Band.Y, false)

  /** @group Constructors */
  case object J extends GpiFilter("J", "J", "J", Band.J, false)

  /** @group Constructors */
  case object H extends GpiFilter("H", "H", "H", Band.H, false)

  /** @group Constructors */
  case object K1 extends GpiFilter("K1", "K1", "K1", Band.K, false)

  /** @group Constructors */
  case object K2 extends GpiFilter("K2", "K2", "K2", Band.K, false)

  /** All members of GpiFilter, in canonical order. */
  val all: List[GpiFilter] =
    List(Y, J, H, K1, K2)

  /** Select the member of GpiFilter with the given tag, if any. */
  def fromTag(s: String): Option[GpiFilter] =
    all.find(_.tag === s)

  /** Select the member of GpiFilter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiFilter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiFilterEnumerated: Enumerated[GpiFilter] =
    new Enumerated[GpiFilter] {
      def all                                          = GpiFilter.all
      def tag(a: GpiFilter)                            = a.tag
      override def unsafeFromTag(s: String): GpiFilter =
        GpiFilter.unsafeFromTag(s)
    }

}
