// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package ifu

import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning

/**
 * Optimal GMOS binning calculation for IFU (Integral Field Unit).
 */
def northBinning: (GmosXBinning, GmosYBinning) =
  binning.northIfuBinning

/**
 * Optimal GMOS binning calculation for IFU (Integral Field Unit).
 */
def southBinning: (GmosXBinning, GmosYBinning) =
  binning.southIfuBinning
