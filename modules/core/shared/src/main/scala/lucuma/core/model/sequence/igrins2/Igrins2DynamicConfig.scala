// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Band
import lucuma.core.enums.Igrins2FowlerSamples
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

case class Igrins2DynamicConfig(
  exposure: TimeSpan
) derives Eq:
  val fowlerSamples: Igrins2FowlerSamples =
    fowlerSamplesForExposureTime(exposure)

  val readoutTime: TimeSpan =
    fowlerSamples.readoutTime

  def readNoise(band: Band): BigDecimal =
    band match
      case Band.H => fowlerSamples.hReadNoise
      case Band.K => fowlerSamples.kReadNoise
      case _      => fowlerSamples.hReadNoise

object Igrins2DynamicConfig:
  /** @group Optics */
  val exposure: Lens[Igrins2DynamicConfig, TimeSpan] =
    Focus[Igrins2DynamicConfig](_.exposure)
