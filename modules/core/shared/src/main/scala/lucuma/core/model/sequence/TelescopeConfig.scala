// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

case class TelescopeConfig(
  offset:  Offset,
  guiding: StepGuideState
)

object TelescopeConfig:

  val Default: TelescopeConfig =
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled)

  /** @group Optics */
  val offset: Lens[TelescopeConfig, Offset] =
    Focus[TelescopeConfig](_.offset)

  /** @group Optics */
  val guiding: Lens[TelescopeConfig, StepGuideState] =
    Focus[TelescopeConfig](_.guiding)

  given Order[TelescopeConfig] = Order.by(t => (t.offset, t.guiding))

  // Explicitly define ordering to avoid auto derivation
  given Ordering[TelescopeConfig] = Order[TelescopeConfig].toOrdering
