// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum TargetDisposition(val tag: String) derives Enumerated:
  case Science     extends TargetDisposition("science")
  case Calibration extends TargetDisposition("calibration")
  case BlindOffset extends TargetDisposition("blind_offset")
