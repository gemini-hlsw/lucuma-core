// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2

import cats.Eq
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

case class F2DynamicConfig(
  exposure:       TimeSpan,
  disperser:      Option[F2Disperser],
  filter:         F2Filter,
  readMode:       F2ReadMode,
  lyot:           F2LyotWheel,
  fpu:            Option[F2Fpu],
  readoutMode:    Option[F2ReadoutMode], // engineering
  reads:          Option[F2Reads], // engineering
) {
  val isImaging: Boolean       = fpu.isEmpty
  val isMOS: Boolean           = fpu.exists(_ === F2Fpu.CustomMask)
  val isLongSlit: Boolean      = fpu.exists(_ =!= F2Fpu.CustomMask)
  val decker: Option[F2Decker] = fpu.map(_.decker)
}

object F2DynamicConfig:
  given Eq[F2DynamicConfig] =
    Eq.by(x => (x.exposure, x.disperser, x.filter, x.readMode, x.lyot, x.fpu, x.readoutMode, x.reads))

  /** @group Optics */
  val exposure: Lens[F2DynamicConfig, TimeSpan] =
    Focus[F2DynamicConfig](_.exposure)

  /** @group Optics */
  val disperser: Lens[F2DynamicConfig, Option[F2Disperser]] =
    Focus[F2DynamicConfig](_.disperser)

  /** @group Optics */
  val filter: Lens[F2DynamicConfig, F2Filter] =
    Focus[F2DynamicConfig](_.filter)

  /** @group Optics */
  val readMode: Lens[F2DynamicConfig, F2ReadMode] =
    Focus[F2DynamicConfig](_.readMode)

  /** @group Optics */
  val lyot: Lens[F2DynamicConfig, F2LyotWheel] =
    Focus[F2DynamicConfig](_.lyot)

  /** @group Optics */
  val fpu: Lens[F2DynamicConfig, Option[F2Fpu]] =
    Focus[F2DynamicConfig](_.fpu)

  /** @group Optics */
  val readoutMode: Lens[F2DynamicConfig, Option[F2ReadoutMode]] =
    Focus[F2DynamicConfig](_.readoutMode)

  /** @group Optics */
  val reads: Lens[F2DynamicConfig, Option[F2Reads]] =
    Focus[F2DynamicConfig](_.reads)
