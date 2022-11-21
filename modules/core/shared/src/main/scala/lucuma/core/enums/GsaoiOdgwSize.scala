// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Gsaoi ODGW Size.
 * @group Enumerations (Generated)
 */
sealed abstract class GsaoiOdgwSize(
  val tag: String,
  val pixels: Int
) extends Product with Serializable

object GsaoiOdgwSize {

  /** @group Constructors */ case object Size4 extends GsaoiOdgwSize("Size4", 4)
  /** @group Constructors */ case object Size6 extends GsaoiOdgwSize("Size6", 6)
  /** @group Constructors */ case object Size8 extends GsaoiOdgwSize("Size8", 8)
  /** @group Constructors */ case object Size16 extends GsaoiOdgwSize("Size16", 16)
  /** @group Constructors */ case object Size32 extends GsaoiOdgwSize("Size32", 32)
  /** @group Constructors */ case object Size64 extends GsaoiOdgwSize("Size64", 64)

  /** All members of GsaoiOdgwSize, in canonical order. */
  val all: List[GsaoiOdgwSize] =
    List(Size4, Size6, Size8, Size16, Size32, Size64)

  /** Select the member of GsaoiOdgwSize with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiOdgwSize] =
    all.find(_.tag === s)

  /** Select the member of GsaoiOdgwSize with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GsaoiOdgwSize =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GsaoiOdgwSize: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GsaoiOdgwSizeEnumerated: Enumerated[GsaoiOdgwSize] =
    new Enumerated[GsaoiOdgwSize] {
      def all = GsaoiOdgwSize.all
      def tag(a: GsaoiOdgwSize) = a.tag
      override def unsafeFromTag(s: String): GsaoiOdgwSize =
        GsaoiOdgwSize.unsafeFromTag(s)
    }

}
