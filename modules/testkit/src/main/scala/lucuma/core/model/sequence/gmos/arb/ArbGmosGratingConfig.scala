// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package arb

import cats.syntax.all.*
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbGmosGratingConfig {
  import ArbEnumerated.given
  import ArbWavelength.given

  given Arbitrary[GmosGratingConfig.North] =
    Arbitrary {
      for {
        grating    <- arbitrary[GmosNorthGrating]
        order      <- arbitrary[GmosGratingOrder]
        wavelength <- arbitrary[Wavelength]
      } yield GmosGratingConfig.North(grating, order, wavelength)
    }

  given Arbitrary[GmosGratingConfig.South] =
    Arbitrary {
      for {
        grating    <- arbitrary[GmosSouthGrating]
        order      <- arbitrary[GmosGratingOrder]
        wavelength <- arbitrary[Wavelength]
      } yield GmosGratingConfig.South(grating, order, wavelength)
    }

  given Arbitrary[GmosGratingConfig] = Arbitrary(
    Gen.oneOf(arbitrary[GmosGratingConfig.North], arbitrary[GmosGratingConfig.South])
  )

  given Cogen[GmosGratingConfig.North] =
    Cogen[(GmosNorthGrating, GmosGratingOrder, Wavelength)].contramap(g =>
      (g.grating, g.order, g.wavelength)
    )

  given Cogen[GmosGratingConfig.South] =
    Cogen[(GmosSouthGrating, GmosGratingOrder, Wavelength)].contramap(g =>
      (g.grating, g.order, g.wavelength)
    )

  given Cogen[GmosGratingConfig] =
    Cogen[Either[GmosGratingConfig.North, GmosGratingConfig.South]]
      .contramap {
        case g @ GmosGratingConfig.North(_, _, _) => g.asLeft
        case g @ GmosGratingConfig.South(_, _, _) => g.asRight
      }
}

object ArbGmosGratingConfig extends ArbGmosGratingConfig
