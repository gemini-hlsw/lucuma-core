// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package ifu

import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning

def northBinning: (GmosXBinning, GmosYBinning) =
  binning.ifuBinning

def southBinning: (GmosXBinning, GmosYBinning) =
  binning.ifuBinning
