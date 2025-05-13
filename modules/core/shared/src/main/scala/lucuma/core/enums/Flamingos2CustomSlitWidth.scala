// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.option.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 custom slit width.
 * @group Enumerations
 */
enum Flamingos2CustomSlitWidth(val tag: String, val shortName: String, val longName: String, val fpu: Option[Flamingos2Fpu])
  derives Enumerated:

  case CustomWidth_1_pix extends Flamingos2CustomSlitWidth("CustomWidth_1_pix", "1 pix", "CustomWidth 1 Pixel", Flamingos2Fpu.LongSlit1.some)
  case CustomWidth_2_pix extends Flamingos2CustomSlitWidth("CustomWidth_2_pix", "2 pix", "CustomWidth 2 Pixel", Flamingos2Fpu.LongSlit2.some)
  case CustomWidth_3_pix extends Flamingos2CustomSlitWidth("CustomWidth_3_pix", "3 pix", "CustomWidth 3 Pixel", Flamingos2Fpu.LongSlit3.some)
  case CustomWidth_4_pix extends Flamingos2CustomSlitWidth("CustomWidth_4_pix", "4 pix", "CustomWidth 4 Pixel", Flamingos2Fpu.LongSlit4.some)
  case CustomWidth_6_pix extends Flamingos2CustomSlitWidth("CustomWidth_6_pix", "6 pix", "CustomWidth 6 Pixel", Flamingos2Fpu.LongSlit6.some)
  case CustomWidth_8_pix extends Flamingos2CustomSlitWidth("CustomWidth_8_pix", "8 pix", "CustomWidth 8 Pixel", Flamingos2Fpu.LongSlit8.some)
  case Other             extends Flamingos2CustomSlitWidth("Other",             "Other", "Other",               none)

