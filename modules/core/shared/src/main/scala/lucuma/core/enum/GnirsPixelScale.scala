// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS Pixel Scale.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsPixelScale(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: BigDecimal
) extends Product with Serializable

object GnirsPixelScale {

  /** @group Constructors */ case object PixelScale_0_05 extends GnirsPixelScale("PixelScale_0_05", "0.05 as/pix", "Pixel scale for short cameras", 0.05)
  /** @group Constructors */ case object PixelScale_0_15 extends GnirsPixelScale("PixelScale_0_15", "0.15 as/pix", "Pixel scale for long cameras", 0.15)

  /** All members of GnirsPixelScale, in canonical order. */
  val all: List[GnirsPixelScale] =
    List(PixelScale_0_05, PixelScale_0_15)

  /** Select the member of GnirsPixelScale with the given tag, if any. */
  def fromTag(s: String): Option[GnirsPixelScale] =
    all.find(_.tag === s)

  /** Select the member of GnirsPixelScale with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsPixelScale =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GnirsPixelScale: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GnirsPixelScaleEnumerated: Enumerated[GnirsPixelScale] =
    new Enumerated[GnirsPixelScale] {
      def all = GnirsPixelScale.all
      def tag(a: GnirsPixelScale) = a.tag
      override def unsafeFromTag(s: String): GnirsPixelScale =
        GnirsPixelScale.unsafeFromTag(s)
    }

}
