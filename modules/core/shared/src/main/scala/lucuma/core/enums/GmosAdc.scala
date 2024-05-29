// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS ADC.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosAdc(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosAdc {

  /** @group Constructors */ case object BestStatic extends GmosAdc("BestStatic", "Best Static", "Best Static Correction")
  /** @group Constructors */ case object Follow extends GmosAdc("Follow", "Follow", "Follow During Exposure")

  /** All members of GmosAdc, in canonical order. */
  val all: List[GmosAdc] =
    List(BestStatic, Follow)

  /** Select the member of GmosAdc with the given tag, if any. */
  def fromTag(s: String): Option[GmosAdc] =
    all.find(_.tag === s)

  /** Select the member of GmosAdc with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosAdc =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosAdc: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  given Enumerated[GmosAdc] =
    new Enumerated[GmosAdc] {
      def all = GmosAdc.all
      def tag(a: GmosAdc) = a.tag
      override def unsafeFromTag(s: String): GmosAdc =
        GmosAdc.unsafeFromTag(s)
    }

}
