// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.CommutativeMonoid
import cats.syntax.order.*

/**
 * Background calculation status.
 */
enum CalculationPhase(val tag: String) derives Enumerated:

  /**
   * Like 'Pending' but 'Retry' signifies that at least one attempt to perform
   * the calculation has previously failed.
   */
  case Retry       extends CalculationPhase("retry")

  /**
   * Pending means an update has marked an observation invalid but no workers
   * have started calculating results.
   */
  case Pending     extends CalculationPhase("pending")

  /**
   * An entry in the 'Calculating' state is being processed by a worker.
   */
  case Calculating extends CalculationPhase("calculating")

  /**
   * Ready signifies that all update computations have completed and the
   * result is not stale.
   */
  case Ready       extends CalculationPhase("ready")

object CalculationPhase:
  val Zero: CalculationPhase =
    CalculationPhase.Ready

  given CommutativeMonoid[CalculationPhase] =
    CommutativeMonoid.instance(Zero, (a, b) => a min b)