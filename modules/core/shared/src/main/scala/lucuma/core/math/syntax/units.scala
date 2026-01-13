// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.syntax

import coulomb.Quantity
import coulomb.syntax.*
import lucuma.core.math.units.*
import coulomb.units.accepted.Millimeter
import coulomb.units.accepted.*

trait ToUnitsOps {
  extension(v: BigDecimal)
    inline def mm: Quantity[BigDecimal, Millimeter] =
      v.withUnit[Millimeter]

    inline def arcsecs: Quantity[BigDecimal, ArcSecond] =
      v.withUnit[ArcSecond]

    inline def plateScale: PlateScale =
      v.withUnit[ArcSecondPerMillimeter]

    inline def pixelScale: PixelScale =
      v.withUnit[ArcSecondPerPixel]

  extension(v: Int)
    inline def pixels: Quantity[Int, Pixels] =
      v.withUnit[Pixels]
}

object units extends ToUnitsOps
