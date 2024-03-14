// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum SlewStage(val tag: String, val name: String, val description: String) derives Enumerated {

  case Start extends SlewStage("start", "Slew Start", "Slew started.")
  case End   extends SlewStage("stop",  "Stop Stop",  "Slew stopped.")

}