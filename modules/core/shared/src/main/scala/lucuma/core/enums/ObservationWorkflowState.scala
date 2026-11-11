// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum ObservationWorkflowState(val tag: String, val name: String) derives Enumerated, Display:

  /**
   * Set aside before execution began, and not to be considered for execution
   * unless and until activated.
   */
  case Inactive   extends ObservationWorkflowState("inactive", "Inactive")

  /**
   * Missing or invalid information keeps the observation from being executed.
   */
  case Undefined  extends ObservationWorkflowState("undefined", "Undefined")

  /** Fully defined, but awaiting approval of its configuration. */
  case Unapproved extends ObservationWorkflowState("unapproved", "Unapproved")

  /** Fully defined and approved, but not yet marked ready for execution. */
  case Defined    extends ObservationWorkflowState("defined", "Defined")

  /** Marked ready, and available to be executed. */
  case Ready      extends ObservationWorkflowState("ready", "Ready")

  /** Execution has started but not yet finished. */
  case Ongoing    extends ObservationWorkflowState("ongoing", "Ongoing")

  /**
   * Started but not completed, and not to be considered for further execution
   * unless and until resumed.
   */
  case Suspended  extends ObservationWorkflowState("suspended", "Suspended")

  /** Execution has finished, whether naturally or by declaration. */
  case Completed  extends ObservationWorkflowState("completed", "Completed")
