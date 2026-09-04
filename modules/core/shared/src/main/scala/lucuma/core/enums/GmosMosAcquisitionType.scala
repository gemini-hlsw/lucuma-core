// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Whether a GMOS MOS acquisition image is taken with the mask in or out.
 */
enum GmosMosAcquisitionType(val tag: String) derives Enumerated:
  case MaskIn  extends GmosMosAcquisitionType("MaskIn")
  case MaskOut extends GmosMosAcquisitionType("MaskOut")
