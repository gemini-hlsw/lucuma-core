// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.syntax.refined.*
import monocle.Focus
import monocle.Lens

/**
 * Number of steps of a sequence that have been executed, split by outcome.
 * A step is successful when it completed or was stopped and every one of its
 * datasets passes QA or has not yet been assessed.  It is unsuccessful
 * otherwise: it was aborted or abandoned, or one of its datasets was marked
 * Usable or Fail.
 *
 * @param successful   executed steps that completed and produced data that
 *                     passes QA (or has not yet been assessed)
 * @param unsuccessful executed steps that did not complete, or produced data
 *                     that does not pass QA
 */
case class ExecutedSteps(
  successful:   NonNegInt,
  unsuccessful: NonNegInt
) derives Eq:

  /** All executed steps, successful or not.  Saturates rather than overflows. */
  def total: NonNegInt =
    successful +| unsuccessful

object ExecutedSteps:

  val Zero: ExecutedSteps =
    ExecutedSteps(NonNegInt.MinValue, NonNegInt.MinValue)

  /**
   * Counts saturate rather than overflow.
   */
  given Monoid[ExecutedSteps] =
    Monoid.instance(
      Zero,
      (a, b) => ExecutedSteps(a.successful +| b.successful, a.unsuccessful +| b.unsuccessful)
    )

  /** @group Optics */
  val successful: Lens[ExecutedSteps, NonNegInt] =
    Focus[ExecutedSteps](_.successful)

  /** @group Optics */
  val unsuccessful: Lens[ExecutedSteps, NonNegInt] =
    Focus[ExecutedSteps](_.unsuccessful)
