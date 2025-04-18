// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum CalibrationRole(val tag: String) derives Enumerated:
  case Twilight           extends CalibrationRole("twilight")
  case Photometric        extends CalibrationRole("photometric")
  case SpectroPhotometric extends CalibrationRole("spectrophotometric")
  case Telluric           extends CalibrationRole("telluric")
