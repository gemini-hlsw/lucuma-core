// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GPI Disperser.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiDisperser(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GpiDisperser {

  /** @group Constructors */ case object PRISM extends GpiDisperser("PRISM", "Prism", "Prism")
  /** @group Constructors */ case object WOLLASTON extends GpiDisperser("WOLLASTON", "Wollaston", "Wollaston")

  /** All members of GpiDisperser, in canonical order. */
  val all: List[GpiDisperser] =
    List(PRISM, WOLLASTON)

  /** Select the member of GpiDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GpiDisperser] =
    all.find(_.tag === s)

  /** Select the member of GpiDisperser with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiDisperser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiDisperser: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiDisperserEnumerated: Enumerated[GpiDisperser] =
    new Enumerated[GpiDisperser] {
      def all = GpiDisperser.all
      def tag(a: GpiDisperser) = a.tag
      override def unsafeFromTag(s: String): GpiDisperser =
        GpiDisperser.unsafeFromTag(s)
    }

}
