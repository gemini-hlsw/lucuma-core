// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

case class TelescopeConfigAlongSlit(
  offset:  Offset.Q,
  guiding: StepGuideState
):
  def toTelescopeConfig: TelescopeConfig =
    TelescopeConfig(Offset(Offset.P.Zero, offset), guiding)

object TelescopeConfigAlongSlit:

  val Default: TelescopeConfigAlongSlit =
    TelescopeConfigAlongSlit(Offset.Q.Zero, StepGuideState.Enabled)

  /** @group Optics */
  val offset: Lens[TelescopeConfigAlongSlit, Offset.Q] =
    Focus[TelescopeConfigAlongSlit](_.offset)

  /** @group Optics */
  val guiding: Lens[TelescopeConfigAlongSlit, StepGuideState] =
    Focus[TelescopeConfigAlongSlit](_.guiding)

  given Order[TelescopeConfigAlongSlit] = Order.by(t => (t.offset, t.guiding))
