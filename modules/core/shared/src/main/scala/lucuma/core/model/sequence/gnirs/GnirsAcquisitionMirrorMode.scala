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

object GnirsGratingWavelength extends NewType[Wavelength]
type GnirsGratingWavelength = GnirsGratingWavelength.Type

enum GnirsAcquisitionMirrorMode(val acquisitionMirror: GnirsAcquisitionMirror) derives Eq:
  case In extends GnirsAcquisitionMirrorMode(GnirsAcquisitionMirror.In)
  case Out(prism: GnirsPrism, grating: GnirsGrating, wavelength: GnirsGratingWavelength)
      extends GnirsAcquisitionMirrorMode(GnirsAcquisitionMirror.Out)
