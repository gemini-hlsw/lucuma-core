// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package ifu

import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning

/**
  * Optimal GMOS binning calculation for IFU (Integral Field Unit) mode.
  * IFU observations always use 1x1 binning to maintain spatial resolution
  * required for proper reconstruction of the integral field.
  */
val ifuBinning: (GmosXBinning, GmosYBinning) =
  (GmosXBinning.One, GmosYBinning.One)

val northBinning: (GmosXBinning, GmosYBinning) =
  ifuBinning

val southBinning: (GmosXBinning, GmosYBinning) =
  ifuBinning
