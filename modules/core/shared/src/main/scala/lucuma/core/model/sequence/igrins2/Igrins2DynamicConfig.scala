// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Igrins2FowlerSamples
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

case class Igrins2DynamicConfig(
  exposure: TimeSpan
) derives Eq:
  val fowlerSamples: Igrins2FowlerSamples =
    val seconds  = exposure.toSeconds.toDouble
    val nFowler  = ((seconds - 1.45479 - 0.168) / 1.45479).toInt
    Igrins2FowlerSamples.values.reverse
      .find(fs => nFowler >= (1 << fs.ordinal))
      .getOrElse(Igrins2FowlerSamples.One)

object Igrins2DynamicConfig:
  /** @group Optics */
  val exposure: Lens[Igrins2DynamicConfig, TimeSpan] =
    Focus[Igrins2DynamicConfig](_.exposure)
