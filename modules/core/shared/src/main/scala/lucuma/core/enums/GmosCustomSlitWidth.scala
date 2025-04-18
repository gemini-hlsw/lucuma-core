// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS custom slit width.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosCustomSlitWidth(
  val tag: String,
  val shortName: String,
  val longName: String,
  val width: Angle
) extends Product with Serializable

object GmosCustomSlitWidth {

  /** @group Constructors */ case object CustomWidth_0_25 extends GmosCustomSlitWidth("CustomWidth_0_25", "0.25\"", "0.25 arcsec", Angle.fromDoubleArcseconds(0.25))
  /** @group Constructors */ case object CustomWidth_0_50 extends GmosCustomSlitWidth("CustomWidth_0_50", "0.50\"", "0.50 arcsec", Angle.fromDoubleArcseconds(0.50))
  /** @group Constructors */ case object CustomWidth_0_75 extends GmosCustomSlitWidth("CustomWidth_0_75", "0.75\"", "0.75 arcsec", Angle.fromDoubleArcseconds(0.75))
  /** @group Constructors */ case object CustomWidth_1_00 extends GmosCustomSlitWidth("CustomWidth_1_00", "1.00\"", "1.00 arcsec", Angle.fromDoubleArcseconds(1.00))
  /** @group Constructors */ case object CustomWidth_1_50 extends GmosCustomSlitWidth("CustomWidth_1_50", "1.50\"", "1.50 arcsec", Angle.fromDoubleArcseconds(1.50))
  /** @group Constructors */ case object CustomWidth_2_00 extends GmosCustomSlitWidth("CustomWidth_2_00", "2.00\"", "2.00 arcsec", Angle.fromDoubleArcseconds(2.00))
  /** @group Constructors */ case object CustomWidth_5_00 extends GmosCustomSlitWidth("CustomWidth_5_00", "5.00\"", "5.00 arcsec", Angle.fromDoubleArcseconds(5.00))

  /** All members of GmosCustomSlitWidth, in canonical order. */
  val all: List[GmosCustomSlitWidth] =
    List(CustomWidth_0_25, CustomWidth_0_50, CustomWidth_0_75, CustomWidth_1_00, CustomWidth_1_50, CustomWidth_2_00, CustomWidth_5_00)

  /** Select the member of GmosCustomSlitWidth with the given tag, if any. */
  def fromTag(s: String): Option[GmosCustomSlitWidth] =
    all.find(_.tag === s)

  /** Select the member of GmosCustomSlitWidth with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosCustomSlitWidth =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosCustomSlitWidth: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosCustomSlitWidthEnumerated: Enumerated[GmosCustomSlitWidth] =
    new Enumerated[GmosCustomSlitWidth] {
      def all = GmosCustomSlitWidth.all
      def tag(a: GmosCustomSlitWidth) = a.tag
      override def unsafeFromTag(s: String): GmosCustomSlitWidth =
        GmosCustomSlitWidth.unsafeFromTag(s)
    }

}
