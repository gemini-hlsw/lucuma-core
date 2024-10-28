// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.derived.*
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

case class TelescopeConfig(
  offset:  Offset,
  guiding: StepGuideState
) derives Order

object TelescopeConfig:

  /** @group Optics */
  val offset: Lens[TelescopeConfig, Offset] =
    Focus[TelescopeConfig](_.offset)

  /** @group Optics */
  val guiding: Lens[TelescopeConfig, StepGuideState] =
    Focus[TelescopeConfig](_.guiding)
