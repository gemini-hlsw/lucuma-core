// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs.arb

import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.math.arb.ArbWavelength.given
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGnirsAcquisitionMirrorMode:
  given Arbitrary[GnirsAcquisitionMirrorMode.Out] = Arbitrary:
    for
      prism      <- arbitrary[GnirsPrism]
      grating    <- arbitrary[GnirsGrating]
      wavelength <- arbitrary[GnirsGratingWavelength]
    yield GnirsAcquisitionMirrorMode.Out(prism, grating, wavelength)

  given Arbitrary[GnirsAcquisitionMirrorMode] = Arbitrary:
    Gen.oneOf(
      Gen.const(GnirsAcquisitionMirrorMode.In),
      arbitrary[GnirsAcquisitionMirrorMode.Out]
    )

  given Cogen[GnirsAcquisitionMirrorMode] =
    Cogen[Option[(GnirsPrism, GnirsGrating, GnirsGratingWavelength)]]
      .contramap:
        case GnirsAcquisitionMirrorMode.In                              => None
        case GnirsAcquisitionMirrorMode.Out(prism, grating, wavelength) =>
          Some((prism, grating, wavelength))

object ArbGnirsAcquisitionMirrorMode extends ArbGnirsAcquisitionMirrorMode
