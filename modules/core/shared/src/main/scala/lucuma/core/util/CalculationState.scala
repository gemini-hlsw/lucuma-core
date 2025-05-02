// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.CommutativeMonoid
import cats.syntax.order.*

/**
 * Background calculation status.
 */
enum CalculationState(val tag: String) derives Enumerated:

  /**
   * Like 'Pending' but signifies that at least one attempt to perform the
   * calculation has previously failed.
   */
  case Retry       extends CalculationState("retry")

  /**
   * Pending means an update has marked a calculated value invalid but no
   * workers have started calculating an updated result.
   */
  case Pending     extends CalculationState("pending")

  /**
   * An entry in the 'Calculating' state is being processed by a worker.
   */
  case Calculating extends CalculationState("calculating")

  /**
   * Ready signifies that all update computations have completed and the
   * result is not stale.
   */
  case Ready       extends CalculationState("ready")

object CalculationState:

  /**
   * The natural 'empty' value for a CalculationState Monoid.  Two states
   * combine such that the least advanced state takes precedence:
   * `Retry` > `Pending` > `Calculating` > `Ready`.  For example,
   * `Ready` |+| 'other' is always 'other' so it is the empty value.
   */
  val Zero: CalculationState =
    CalculationState.Ready

  given CommutativeMonoid[CalculationState] =
    CommutativeMonoid.instance(Zero, (a, b) => a min b)