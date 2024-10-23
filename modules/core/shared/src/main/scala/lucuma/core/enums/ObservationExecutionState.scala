// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * The execution state of an observation.
 */
enum ObservationExecutionState(val tag: String) derives Enumerated:

  /**
   * The observation isn't sufficiently defined, or there is a problem that
   * must first be resolved.
   */
  case NotDefined  extends ObservationExecutionState("not_defined")

  /** No execution visit has been recorded for this observation. */
  case NotStarted  extends ObservationExecutionState("not_started")

  /** At least one visit was made for this observation, but it is not yet complete. */
  case Ongoing     extends ObservationExecutionState("ongoing")

  /** No more science data is expected for this observation. */
  case Completed   extends ObservationExecutionState("completed")