// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength

private[enums] trait ConvenienceOps {
  extension (i: Int) {
    def pm: Wavelength =
      Wavelength.unsafeFromIntPicometers(i)

    def gePmRange: BoundedInterval[Wavelength] =
      // This is open upper (instead of closed) because of a limitation in postgres int4range
      BoundedInterval.unsafeOpenUpper(i.pm, Wavelength.Max)
  }

  extension (tup: (Int, Int)) {
    def pmRange: BoundedInterval[Wavelength] =
      BoundedInterval.unsafeOpenUpper(tup._1.pm, tup._2.pm)
  }

}

private[enums] object ConvenienceOps extends ConvenienceOps
