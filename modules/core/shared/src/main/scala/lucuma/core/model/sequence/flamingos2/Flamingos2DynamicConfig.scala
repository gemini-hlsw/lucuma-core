// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.Eq
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.util.TimeSpan
import lucuma.core.math.Wavelength
import monocle.Focus
import monocle.Lens

case class Flamingos2DynamicConfig(
  exposure:    TimeSpan,
  disperser:   Option[Flamingos2Disperser],
  filter:      Flamingos2Filter,
  readMode:    Flamingos2ReadMode,
  lyotWheel:   Flamingos2LyotWheel,
  fpu:         Flamingos2FpuMask,
  decker:      Flamingos2Decker,
  readoutMode: Flamingos2ReadoutMode,
  reads:       Flamingos2Reads
):
  val isImaging: Boolean  = fpu.isImaging
  val isMOS: Boolean      = fpu.isMOS
  val isLongSlit: Boolean = fpu.isLongSlit

  val centralWavelength: Wavelength =
    disperser.fold(filter.wavelength)(_.wavelength)

object Flamingos2DynamicConfig:
  given Eq[Flamingos2DynamicConfig] =
    Eq.by(x => (x.exposure, x.disperser, x.filter, x.readMode, x.lyotWheel, x.fpu, x.decker, x.readoutMode, x.reads))

  /** @group Optics */
  val exposure: Lens[Flamingos2DynamicConfig, TimeSpan] =
    Focus[Flamingos2DynamicConfig](_.exposure)

  /** @group Optics */
  val disperser: Lens[Flamingos2DynamicConfig, Option[Flamingos2Disperser]] =
    Focus[Flamingos2DynamicConfig](_.disperser)

  /** @group Optics */
  val filter: Lens[Flamingos2DynamicConfig, Flamingos2Filter] =
    Focus[Flamingos2DynamicConfig](_.filter)

  /** @group Optics */
  val readMode: Lens[Flamingos2DynamicConfig, Flamingos2ReadMode] =
    Focus[Flamingos2DynamicConfig](_.readMode)

  /** @group Optics */
  val lyotWheel: Lens[Flamingos2DynamicConfig, Flamingos2LyotWheel] =
    Focus[Flamingos2DynamicConfig](_.lyotWheel)

  /** @group Optics */
  val fpu: Lens[Flamingos2DynamicConfig, Flamingos2FpuMask] =
    Focus[Flamingos2DynamicConfig](_.fpu)

  /** @group Optics */
  val decker: Lens[Flamingos2DynamicConfig, Flamingos2Decker] =
    Focus[Flamingos2DynamicConfig](_.decker)

  /** @group Optics */
  val readoutMode: Lens[Flamingos2DynamicConfig, Flamingos2ReadoutMode] =
    Focus[Flamingos2DynamicConfig](_.readoutMode)

  /** @group Optics */
  val reads: Lens[Flamingos2DynamicConfig, Flamingos2Reads] =
    Focus[Flamingos2DynamicConfig](_.reads)