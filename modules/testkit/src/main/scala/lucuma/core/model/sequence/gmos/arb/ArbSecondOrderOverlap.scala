// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package arb

import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSecondOrderOverlap:
  import ArbWavelength.given

  given Arbitrary[SecondOrderOverlap] =
    Arbitrary:
      for
        o <- arbitrary[Wavelength]
        w <- arbitrary[Wavelength]
      yield SecondOrderOverlap(o, w)

  given Cogen[SecondOrderOverlap] =
    Cogen[(Wavelength, Wavelength)].contramap: a =>
      (a.onset, a.centralWavelength)

object ArbSecondOrderOverlap extends ArbSecondOrderOverlap
