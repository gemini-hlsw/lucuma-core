// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * The execution state of a sequence or observation.
 */
enum ExecutionState(val tag: String) derives Enumerated:

  /**
   * The observation isn't sufficiently defined, or there is a problem that
   * must first be resolved before the execution state may be determined.
   */
  case NotDefined  extends ExecutionState("not_defined")

  /** No execution visit has been recorded for this observation. */
  case NotStarted  extends ExecutionState("not_started")

  /**
   * At least one visit was made for this observation, but the sequence or
   * observation is not yet complete.
   */
  case Ongoing     extends ExecutionState("ongoing")

  /** No more data is expected for this observation or sequence. */
  case Completed   extends ExecutionState("completed")

  /**
   * The observation has been explicitly declared complete by a user even
   * though more atoms and/or steps remain.
   */
  case DeclaredComplete extends ExecutionState("declared_complete")