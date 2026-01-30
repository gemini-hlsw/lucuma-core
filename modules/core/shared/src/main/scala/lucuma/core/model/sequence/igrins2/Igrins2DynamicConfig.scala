// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.Eq
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

case class Igrins2DynamicConfig(
  exposure: TimeSpan
)

object Igrins2DynamicConfig:
  given Eq[Igrins2DynamicConfig] =
    Eq.by(_.exposure)

  /** @group Optics */
  val exposure: Lens[Igrins2DynamicConfig, TimeSpan] =
    Focus[Igrins2DynamicConfig](_.exposure)
