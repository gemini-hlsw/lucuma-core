// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.syntax

import coulomb.Quantity
import coulomb.syntax.*
import lucuma.core.math.units.*

trait ToUnitsOps {
  extension(v: BigDecimal)
    def plateScale: PlateScale =
      v.withUnit[ArcSecondPerMillimeter]

    def pixelScale: PixelScale =
      v.withUnit[ArcSecondPerPixel]

  extension(v: Int)
    def pixels: Quantity[Int, Pixels] =
      v.withUnit[Pixels]
}

object units extends ToUnitsOps
