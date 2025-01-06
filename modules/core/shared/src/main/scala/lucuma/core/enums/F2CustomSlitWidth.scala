// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for F2 custom slit width.
 * @group Enumerations
 */
enum F2CustomSlitWidth(val tag: String, val shortName: String, val longName: String, val fpu: F2Fpu)
  derives Enumerated:

  case CustomWidth_1_pix extends F2CustomSlitWidth("CustomWidth_1_pix", "1 pix", "CustomWidth 1 Pixel", F2Fpu.LongSlit1)
  case CustomWidth_2_pix extends F2CustomSlitWidth("CustomWidth_2_pix", "2 pix", "CustomWidth 2 Pixel", F2Fpu.LongSlit2)
  case CustomWidth_3_pix extends F2CustomSlitWidth("CustomWidth_3_pix", "3 pix", "CustomWidth 3 Pixel", F2Fpu.LongSlit3)
  case CustomWidth_4_pix extends F2CustomSlitWidth("CustomWidth_4_pix", "4 pix", "CustomWidth 4 Pixel", F2Fpu.LongSlit4)
  case CustomWidth_6_pix extends F2CustomSlitWidth("CustomWidth_6_pix", "6 pix", "CustomWidth 6 Pixel", F2Fpu.LongSlit6)
  case CustomWidth_8_pix extends F2CustomSlitWidth("CustomWidth_8_pix", "8 pix", "CustomWidth 8 Pixel", F2Fpu.LongSlit8)
  case Other             extends F2CustomSlitWidth("Other",             "Other", "Other",               F2Fpu.CustomMask)

