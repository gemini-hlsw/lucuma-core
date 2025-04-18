// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Eq
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait DynamicConfig

object DynamicConfig {

  final case class GmosNorth(
    exposure:      TimeSpan,
    readout:       GmosCcdMode,
    dtax:          GmosDtax,
    roi:           GmosRoi,
    gratingConfig: Option[GmosGratingConfig.North],
    filter:        Option[GmosNorthFilter],
    fpu:           Option[GmosFpuMask[GmosNorthFpu]]
  ) extends DynamicConfig {
    val centralWavelength: Option[Wavelength] =
      gratingConfig.fold(filter.map(_.wavelength))(_.wavelength.some)
  }

  object GmosNorth {
    given Eq[GmosNorth] =
      Eq.by(x => (x.exposure, x.readout, x.dtax, x.roi, x.gratingConfig, x.filter, x.fpu))

    /** @group Optics */
    val exposure: Lens[GmosNorth, TimeSpan] =
      Focus[GmosNorth](_.exposure)

    /** @group Optics */
    val readout: Lens[GmosNorth, GmosCcdMode] =
      Focus[GmosNorth](_.readout)

    /** @group Optics */
    val dtax: Lens[GmosNorth, GmosDtax] =
      Focus[GmosNorth](_.dtax)

    /** @group Optics */
    val roi: Lens[GmosNorth, GmosRoi] =
      Focus[GmosNorth](_.roi)

    /** @group Optics */
    val gratingConfig: Lens[GmosNorth, Option[GmosGratingConfig.North]] =
      Focus[GmosNorth](_.gratingConfig)

    /** @group Optics */
    val filter: Lens[GmosNorth, Option[GmosNorthFilter]] =
      Focus[GmosNorth](_.filter)

    /** @group Optics */
    val fpu: Lens[GmosNorth, Option[GmosFpuMask[GmosNorthFpu]]] =
      Focus[GmosNorth](_.fpu)
  }

  final case class GmosSouth(
    exposure:      TimeSpan,
    readout:       GmosCcdMode,
    dtax:          GmosDtax,
    roi:           GmosRoi,
    gratingConfig: Option[GmosGratingConfig.South],
    filter:        Option[GmosSouthFilter],
    fpu:           Option[GmosFpuMask[GmosSouthFpu]]
  ) extends DynamicConfig{
    val centralWavelength: Option[Wavelength] =
      gratingConfig.fold(filter.map(_.wavelength))(_.wavelength.some)
  }

  object GmosSouth {

    given Eq[GmosSouth] =
      Eq.by(x => (x.exposure, x.readout, x.dtax, x.roi, x.gratingConfig, x.filter, x.fpu))

    /** @group Optics */
    val exposure: Lens[GmosSouth, TimeSpan] =
      Focus[GmosSouth](_.exposure)

    /** @group Optics */
    val readout: Lens[GmosSouth, GmosCcdMode] =
      Focus[GmosSouth](_.readout)

    /** @group Optics */
    val dtax: Lens[GmosSouth, GmosDtax] =
      Focus[GmosSouth](_.dtax)

    /** @group Optics */
    val roi: Lens[GmosSouth, GmosRoi] =
      Focus[GmosSouth](_.roi)

    /** @group Optics */
    val gratingConfig: Lens[GmosSouth, Option[GmosGratingConfig.South]] =
      Focus[GmosSouth](_.gratingConfig)

    /** @group Optics */
    val filter: Lens[GmosSouth, Option[GmosSouthFilter]] =
      Focus[GmosSouth](_.filter)

    /** @group Optics */
    val fpu: Lens[GmosSouth, Option[GmosFpuMask[GmosSouthFpu]]] =
      Focus[GmosSouth](_.fpu)
  }

  given Eq[DynamicConfig] = Eq.instance {
    case (a @ GmosNorth(_, _, _, _, _, _, _), b @ GmosNorth(_, _, _, _, _, _, _)) => a === b
    case (a @ GmosSouth(_, _, _, _, _, _, _), b @ GmosSouth(_, _, _, _, _, _, _)) => a === b
    case _                                                                        => false
  }

  /** @group Optics */
  val gmosNorth: Prism[DynamicConfig, GmosNorth] =
    GenPrism[DynamicConfig, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[DynamicConfig, GmosSouth] =
    GenPrism[DynamicConfig, GmosSouth]
}
