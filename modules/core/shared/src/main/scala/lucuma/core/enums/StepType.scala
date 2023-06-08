// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum StepType(val tag: String, val name: String) derives Enumerated {
  case Bias      extends StepType("bias",       "Bias")
  case Dark      extends StepType("dark",       "Dark")
  case Gcal      extends StepType("gcal",       "Gcal")
  case Science   extends StepType("science",    "Science")
  case SmartGcal extends StepType("smart_gcal", "SmartGcal")
}