// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ProgramStatus(val tag: String, val name: String) derives Enumerated:
  case Active     extends ProgramStatus("active",     "Active")
  case Inactive   extends ProgramStatus("inactive",   "Inactive")
  case Complete   extends ProgramStatus("complete",   "Complete")
  case Incomplete extends ProgramStatus("incomplete", "Incomplete")

object ProgramStatus:
  val Default: ProgramStatus = Active
