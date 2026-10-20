// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import lucuma.core.enums.ObservationValidationCode

/**
 * A problem found in a step of a sequence, such as an exposure time outside
 * the instrument's limits, as the validation it raises.  A sequence's digest
 * keeps those its steps raise, so a rule broken in many steps is reported
 * once.  Errors and warnings are distinguished by the code.
 *
 * @param code    validation code reported for the problem
 * @param message e.g. "Exposure times for GMOS North must be at least 1 s."
 */
final case class StepViolation(
  code:    ObservationValidationCode,
  message: String
):

  def isError: Boolean =
    code.isError

object StepViolation:

  /** Errors first, then by code and message. */
  given Order[StepViolation] =
    Order.by(v => (v.code, v.message))

  given Ordering[StepViolation] =
    Order[StepViolation].toOrdering
