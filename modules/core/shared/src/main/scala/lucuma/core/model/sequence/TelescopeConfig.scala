// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

/**
 * The telescope configuration of a step: where the telescope points, whether it
 * guides there, and what the position is for.  Unlike a user-specified
 * `TelescopeConfigSpec`, the purpose is always resolved.
 */
case class TelescopeConfig(
  offset:  Offset,
  guiding: StepGuideState,
  purpose: OffsetPurpose
):
  def toTelescopeConfigSpec: TelescopeConfigSpec =
    TelescopeConfigSpec(offset, guiding, Some(purpose))

object TelescopeConfig:

  val Default: TelescopeConfig =
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled, OffsetPurpose.Object)

  /** @group Optics */
  val offset: Lens[TelescopeConfig, Offset] =
    Focus[TelescopeConfig](_.offset)

  /** @group Optics */
  val guiding: Lens[TelescopeConfig, StepGuideState] =
    Focus[TelescopeConfig](_.guiding)

  /** @group Optics */
  val purpose: Lens[TelescopeConfig, OffsetPurpose] =
    Focus[TelescopeConfig](_.purpose)

  given Order[TelescopeConfig] = Order.by(t => (t.offset, t.guiding, t.purpose))

  // Explicitly define ordering to avoid auto derivation
  given Ordering[TelescopeConfig] = Order[TelescopeConfig].toOrdering
