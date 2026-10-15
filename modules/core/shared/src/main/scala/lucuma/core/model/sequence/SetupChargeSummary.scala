// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.derived.*
import cats.kernel.CommutativeMonoid
import cats.syntax.monoid.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import monocle.Focus
import monocle.Lens

/**
 * Number of setups of a particular kind and the time charged for them.
 */
case class SetupChargeSummary(
  count: NonNegInt,
  time:  SetupTimeBreakdown
) derives Order

object SetupChargeSummary:

  val Zero: SetupChargeSummary =
    SetupChargeSummary(NonNegInt.MinValue, SetupTimeBreakdown.Zero)

  /**
   * Both count and time saturate rather than overflow.
   */
  given CommutativeMonoid[SetupChargeSummary] =
    CommutativeMonoid.instance(
      Zero,
      (a, b) =>
        val count = (a.count.value.toLong + b.count.value).min(Int.MaxValue).toInt
        SetupChargeSummary(NonNegInt.unsafeFrom(count), a.time |+| b.time)
    )

  /** @group Optics */
  val count: Lens[SetupChargeSummary, NonNegInt] =
    Focus[SetupChargeSummary](_.count)

  /** @group Optics */
  val time: Lens[SetupChargeSummary, SetupTimeBreakdown] =
    Focus[SetupChargeSummary](_.time)
