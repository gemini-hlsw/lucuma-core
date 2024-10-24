// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ObservationWorkflowState(val tag: String) derives Enumerated:
  case Inactive   extends ObservationWorkflowState("inactive")
  case Undefined  extends ObservationWorkflowState("undefined")
  case Unapproved extends ObservationWorkflowState("unapproved")
  case Defined    extends ObservationWorkflowState("defined")
  case Ready      extends ObservationWorkflowState("ready")
  case Ongoing    extends ObservationWorkflowState("ongoing")
  case Completed  extends ObservationWorkflowState("completed")



