// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS grating order.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosGratingOrder(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int
) extends Product with Serializable

object GmosGratingOrder {

  /** @group Constructors */ case object Zero extends GmosGratingOrder("Zero", "0", "Zero", 0)
  /** @group Constructors */ case object One  extends GmosGratingOrder("One",  "1", "One",  1)
  /** @group Constructors */ case object Two  extends GmosGratingOrder("Two",  "2", "Two",  2)

  /** All members of GmosDisperserOrder, in canonical order. */
  val all: List[GmosGratingOrder] =
    List(Zero, One, Two)

  /** Select the member of GmosDisperserOrder with the given tag, if any. */
  def fromTag(s: String): Option[GmosGratingOrder] =
    all.find(_.tag === s)

  /** Select the member of GmosDisperserOrder with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosGratingOrder =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosDisperserOrder: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosGratingOrderEnumerated: Enumerated[GmosGratingOrder] =
    new Enumerated[GmosGratingOrder] {
      def all: List[GmosGratingOrder] = GmosGratingOrder.all
      def tag(a: GmosGratingOrder): String = a.tag
      override def unsafeFromTag(s: String): GmosGratingOrder =
        GmosGratingOrder.unsafeFromTag(s)
    }

}
