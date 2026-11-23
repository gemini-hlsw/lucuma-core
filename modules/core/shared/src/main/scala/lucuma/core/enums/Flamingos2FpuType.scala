// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum Flamingos2FpuType(val tag: String) derives Enumerated:
  case LongSlit extends Flamingos2FpuType("LongSlit")
  case Pinhole  extends Flamingos2FpuType("Pinhole")
  case MOS      extends Flamingos2FpuType("MOS")
