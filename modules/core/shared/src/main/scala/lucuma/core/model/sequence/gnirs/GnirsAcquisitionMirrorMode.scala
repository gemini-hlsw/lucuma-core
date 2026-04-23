// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GnirsAcquisitionMirror
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.math.Wavelength
import lucuma.core.util.NewType
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism


object GnirsGratingWavelength extends NewType[Wavelength]
type GnirsGratingWavelength = GnirsGratingWavelength.Type

enum GnirsAcquisitionMirrorMode(val acquisitionMirror: GnirsAcquisitionMirror) derives Eq:
  case In extends GnirsAcquisitionMirrorMode(GnirsAcquisitionMirror.In)
  case Out(prism: GnirsPrism, grating: GnirsGrating, wavelength: GnirsGratingWavelength)
      extends GnirsAcquisitionMirrorMode(GnirsAcquisitionMirror.Out)

object GnirsAcquisitionMirrorMode:
  val in: Prism[GnirsAcquisitionMirrorMode, In.type] =
    GenPrism[GnirsAcquisitionMirrorMode, In.type]

  val out: Prism[GnirsAcquisitionMirrorMode, Out] =
    GenPrism[GnirsAcquisitionMirrorMode, Out]

  object Out:
    val prism: Lens[Out, GnirsPrism] =
      Focus[Out](_.prism)

    val grating: Lens[Out, GnirsGrating] =
      Focus[Out](_.grating)

    val wavelength: Lens[Out, GnirsGratingWavelength] =
      Focus[Out](_.wavelength)

