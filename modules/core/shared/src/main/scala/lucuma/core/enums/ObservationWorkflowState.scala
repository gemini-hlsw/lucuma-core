// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum ObservationWorkflowState(val tag: String, val name: String) derives Enumerated, Display:
  case Inactive   extends ObservationWorkflowState("inactive", "Inactive")
  case Undefined  extends ObservationWorkflowState("undefined", "Undefined")
  case Unapproved extends ObservationWorkflowState("unapproved", "Unapproved")
  case Defined    extends ObservationWorkflowState("defined", "Defined")
  case Ready      extends ObservationWorkflowState("ready", "Ready")
  case Ongoing    extends ObservationWorkflowState("ongoing", "Ongoing")
  case Completed  extends ObservationWorkflowState("completed", "Completed")



