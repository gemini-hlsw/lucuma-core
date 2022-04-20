// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.enum.GmosGratingOrder
import lucuma.core.enum.GmosNorthGrating
import lucuma.core.enum.GmosSouthGrating
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.sequence.GmosGrating
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbGmosGrating {
  import ArbEnumerated._
  import ArbWavelength._

  implicit val arbGmosNorthGrating: Arbitrary[GmosGrating.North] =
    Arbitrary {
      for {
        grating    <- arbitrary[GmosNorthGrating]
        order      <- arbitrary[GmosGratingOrder]
        wavelength <- arbitrary[Wavelength]
      } yield GmosGrating.North(grating, order, wavelength)
    }

  implicit val arbGmosSouthGrating: Arbitrary[GmosGrating.South] =
    Arbitrary {
      for {
        grating    <- arbitrary[GmosSouthGrating]
        order      <- arbitrary[GmosGratingOrder]
        wavelength <- arbitrary[Wavelength]
      } yield GmosGrating.South(grating, order, wavelength)
    }

  implicit val arbGmosGrating: Arbitrary[GmosGrating] = Arbitrary(
    Gen.oneOf(arbitrary[GmosGrating.North], arbitrary[GmosGrating.South])
  )

  implicit val cogGmosNorthGrating: Cogen[GmosGrating.North] =
    Cogen[(GmosNorthGrating, GmosGratingOrder, Wavelength)].contramap(g =>
      (g.grating, g.order, g.wavelength)
    )

  implicit val cogGmosSouthGrating: Cogen[GmosGrating.South] =
    Cogen[(GmosSouthGrating, GmosGratingOrder, Wavelength)].contramap(g =>
      (g.grating, g.order, g.wavelength)
    )

  implicit val cogGmosGrating: Cogen[GmosGrating] =
    Cogen[Either[GmosGrating.North, GmosGrating.South]]
      .contramap {
        case g @ GmosGrating.North(_, _, _) => g.asLeft
        case g @ GmosGrating.South(_, _, _) => g.asRight
      }
}

object ArbGmosGrating extends ArbGmosGrating
