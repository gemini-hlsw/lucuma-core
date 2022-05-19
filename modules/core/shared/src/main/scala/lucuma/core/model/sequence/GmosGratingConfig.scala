// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import lucuma.core.`enum`.GmosGratingOrder
import lucuma.core.`enum`.GmosNorthGrating
import lucuma.core.`enum`.GmosSouthGrating
import lucuma.core.math.Wavelength
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait GmosGratingConfig
object GmosGratingConfig {
  final case class North(
    grating:    GmosNorthGrating,
    order:      GmosGratingOrder,
    wavelength: Wavelength
  ) extends GmosGratingConfig
  object North {
    implicit val eqGmosGratingNorth: Eq[North] = Eq.by(x => (x.grating, x.order, x.wavelength))

    /** @group Optics */
    val grating: Lens[North, GmosNorthGrating] =
      Focus[North](_.grating)

    /** @group Optics */
    val order: Lens[North, GmosGratingOrder] =
      Focus[North](_.order)

    /** @group Optics */
    val wavelength: Lens[North, Wavelength] =
      Focus[North](_.wavelength)
  }

  final case class South(
    grating:    GmosSouthGrating,
    order:      GmosGratingOrder,
    wavelength: Wavelength
  ) extends GmosGratingConfig
  object South {
    implicit val eqGmosGratingSouth: Eq[South] = Eq.by(x => (x.grating, x.order, x.wavelength))

    /** @group Optics */
    val grating: Lens[South, GmosSouthGrating] =
      Focus[South](_.grating)

    /** @group Optics */
    val order: Lens[South, GmosGratingOrder] =
      Focus[South](_.order)

    /** @group Optics */
    val wavelength: Lens[South, Wavelength] =
      Focus[South](_.wavelength)
  }

  implicit val eqGmosGrating: Eq[GmosGratingConfig] = Eq.instance {
    case (a @ North(_, _, _), b @ North(_, _, _)) => a === b
    case (a @ South(_, _, _), b @ South(_, _, _)) => a === b
    case _                                        => false
  }

  /** @group Optics */
  val north: Prism[GmosGratingConfig, GmosGratingConfig.North] =
    GenPrism[GmosGratingConfig, GmosGratingConfig.North]

  /** @group Optics */
  val south: Prism[GmosGratingConfig, GmosGratingConfig.South] =
    GenPrism[GmosGratingConfig, GmosGratingConfig.South]
}
