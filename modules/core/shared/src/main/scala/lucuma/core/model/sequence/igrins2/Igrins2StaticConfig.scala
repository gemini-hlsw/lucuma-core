// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Igrins2OffsetMode
import monocle.Focus
import monocle.Lens

case class Igrins2StaticConfig(
  saveSVCImages: Igrins2SVCImages,
  offsetMode:    Igrins2OffsetMode
) derives Eq

object Igrins2StaticConfig:
  /** @group Optics */
  val saveSVCImages: Lens[Igrins2StaticConfig, Igrins2SVCImages] =
    Focus[Igrins2StaticConfig](_.saveSVCImages)

  /** @group Optics */
  val offsetMode: Lens[Igrins2StaticConfig, Igrins2OffsetMode] =
    Focus[Igrins2StaticConfig](_.offsetMode)
