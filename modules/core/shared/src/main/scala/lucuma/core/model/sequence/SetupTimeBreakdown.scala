// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.derived.*
import cats.kernel.CommutativeMonoid
import cats.syntax.monoid.*
import monocle.Focus
import monocle.Lens

/**
 * Time charged for setting up on a target, broken down into its components.
 * Setup ends when the first science step starts.
 *
 * @param slew        time spent slewing
 * @param acquisition time spent executing the acquisition sequence proper
 * @param other       remaining setup time, neither slewing nor executing the
 *                    acquisition sequence (e.g., the lead-in to the
 *                    acquisition and the hand-off to science)
 */
case class SetupTimeBreakdown(
  slew:        CategorizedTime,
  acquisition: CategorizedTime,
  other:       CategorizedTime
) derives Order:

  /** Total setup time: slew, acquisition and other time combined. */
  def total: CategorizedTime =
    slew |+| acquisition |+| other

object SetupTimeBreakdown:

  val Zero: SetupTimeBreakdown =
    SetupTimeBreakdown(CategorizedTime.Zero, CategorizedTime.Zero, CategorizedTime.Zero)

  given CommutativeMonoid[SetupTimeBreakdown] =
    CommutativeMonoid.instance(
      Zero,
      (a, b) => SetupTimeBreakdown(a.slew |+| b.slew, a.acquisition |+| b.acquisition, a.other |+| b.other)
    )

  /** @group Optics */
  val slew: Lens[SetupTimeBreakdown, CategorizedTime] =
    Focus[SetupTimeBreakdown](_.slew)

  /** @group Optics */
  val acquisition: Lens[SetupTimeBreakdown, CategorizedTime] =
    Focus[SetupTimeBreakdown](_.acquisition)

  /** @group Optics */
  val other: Lens[SetupTimeBreakdown, CategorizedTime] =
    Focus[SetupTimeBreakdown](_.other)
