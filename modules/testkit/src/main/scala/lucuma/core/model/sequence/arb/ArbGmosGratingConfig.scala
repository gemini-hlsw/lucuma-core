// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.sequence.GmosGratingConfig
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbGmosGratingConfig {
  import ArbEnumerated._
  import ArbWavelength._

  implicit val arbGmosNorthGratingConfig: Arbitrary[GmosGratingConfig.North] =
    Arbitrary {
      for {
        grating    <- arbitrary[GmosNorthGrating]
        order      <- arbitrary[GmosGratingOrder]
        wavelength <- arbitrary[Wavelength]
      } yield GmosGratingConfig.North(grating, order, wavelength)
    }

  implicit val arbGmosSouthGratingConfig: Arbitrary[GmosGratingConfig.South] =
    Arbitrary {
      for {
        grating    <- arbitrary[GmosSouthGrating]
        order      <- arbitrary[GmosGratingOrder]
        wavelength <- arbitrary[Wavelength]
      } yield GmosGratingConfig.South(grating, order, wavelength)
    }

  implicit val arbGmosGratingConfig: Arbitrary[GmosGratingConfig] = Arbitrary(
    Gen.oneOf(arbitrary[GmosGratingConfig.North], arbitrary[GmosGratingConfig.South])
  )

  implicit val cogGmosNorthGratingConfig: Cogen[GmosGratingConfig.North] =
    Cogen[(GmosNorthGrating, GmosGratingOrder, Wavelength)].contramap(g =>
      (g.grating, g.order, g.wavelength)
    )

  implicit val cogGmosSouthGratingConfig: Cogen[GmosGratingConfig.South] =
    Cogen[(GmosSouthGrating, GmosGratingOrder, Wavelength)].contramap(g =>
      (g.grating, g.order, g.wavelength)
    )

  implicit val cogGmosGratingConfig: Cogen[GmosGratingConfig] =
    Cogen[Either[GmosGratingConfig.North, GmosGratingConfig.South]]
      .contramap {
        case g @ GmosGratingConfig.North(_, _, _) => g.asLeft
        case g @ GmosGratingConfig.South(_, _, _) => g.asRight
      }
}

object ArbGmosGratingConfig extends ArbGmosGratingConfig
