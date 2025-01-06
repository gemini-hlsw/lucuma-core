// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for F2 custom slit width.
 * @group Enumerations
 */
sealed abstract class F2CustomSlitWidth(
  val tag: String,
  val shortName: String,
  val longName: String,
  val fpu: F2Fpu
) extends Product with Serializable

object F2CustomSlitWidth:

  /** @group Constructors */ case object CustomWidth_1_pix extends F2CustomSlitWidth("CustomWidth_1_pix", "1 pix", "CustomWidth 1 Pixel", F2Fpu.LongSlit1)
  /** @group Constructors */ case object CustomWidth_2_pix extends F2CustomSlitWidth("CustomWidth_2_pix", "2 pix", "CustomWidth 2 Pixel", F2Fpu.LongSlit2)
  /** @group Constructors */ case object CustomWidth_3_pix extends F2CustomSlitWidth("CustomWidth_3_pix", "3 pix", "CustomWidth 3 Pixel", F2Fpu.LongSlit3)
  /** @group Constructors */ case object CustomWidth_4_pix extends F2CustomSlitWidth("CustomWidth_4_pix", "4 pix", "CustomWidth 4 Pixel", F2Fpu.LongSlit4)
  /** @group Constructors */ case object CustomWidth_6_pix extends F2CustomSlitWidth("CustomWidth_6_pix", "6 pix", "CustomWidth 6 Pixel", F2Fpu.LongSlit6)
  /** @group Constructors */ case object CustomWidth_8_pix extends F2CustomSlitWidth("CustomWidth_8_pix", "8 pix", "CustomWidth 8 Pixel", F2Fpu.LongSlit8)
  /** @group Constructors */ case object Other extends F2CustomSlitWidth("Other", "Other", "Other", F2Fpu.CustomMask)

  /** All members of F2CustomSlitWidth, in canonical order. */
  val all: List[F2CustomSlitWidth] =
    List(CustomWidth_1_pix, CustomWidth_2_pix, CustomWidth_3_pix, CustomWidth_4_pix, CustomWidth_6_pix, CustomWidth_8_pix, Other)

  /** Select the member of F2CustomSlitWidth with the given tag, if any. */
  def fromTag(s: String): Option[F2CustomSlitWidth] =
    all.find(_.tag === s)

  /** Select the member of F2CustomSlitWidth with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): F2CustomSlitWidth =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"F2CustomSlitWidth: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  given Enumerated[F2CustomSlitWidth] =
    new Enumerated[F2CustomSlitWidth]:
      def all = F2CustomSlitWidth.all
      def tag(a: F2CustomSlitWidth) = a.tag
      override def unsafeFromTag(s: String): F2CustomSlitWidth =
        F2CustomSlitWidth.unsafeFromTag(s)

