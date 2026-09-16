// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.arb

import eu.timepit.refined.scalacheck.numeric.*
import lucuma.catalog.BandsList
import lucuma.catalog.BrightnessConstraints
import lucuma.catalog.FaintnessConstraint
import lucuma.catalog.SaturationConstraint
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.util.arb.ArbNewType.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbBrightnessConstraints {

  given Arbitrary[FaintnessConstraint] =
    Arbitrary {
      for {
        b <- arbitrary[BrightnessValue]
      } yield FaintnessConstraint(b)
    }

  given Cogen[FaintnessConstraint] =
    Cogen[BrightnessValue].contramap(_.brightness)

  given Arbitrary[SaturationConstraint] =
    Arbitrary {
      for {
        b <- arbitrary[BrightnessValue]
      } yield SaturationConstraint(b)
    }

  given Cogen[SaturationConstraint] =
    Cogen[BrightnessValue].contramap(_.brightness)

  given Arbitrary[BandsList] =
    Arbitrary(Gen.oneOf(BandsList.GaiaBandsList, BandsList.RBandsList))

  given Cogen[BandsList] =
    Cogen[List[String]].contramap(_.bands.map(_.tag))

  given Arbitrary[BrightnessConstraints] =
    Arbitrary {
      for {
        b <- arbitrary[BandsList]
        f <- arbitrary[FaintnessConstraint]
        l <- arbitrary[Option[SaturationConstraint]]
      } yield BrightnessConstraints(b, f, l)
    }

  given Cogen[BrightnessConstraints] =
    Cogen[(BandsList, FaintnessConstraint, Option[SaturationConstraint])].contramap(x =>
      (x.searchBands, x.faintnessConstraint, x.saturationConstraint)
    )
}

object ArbBrightnessConstraints extends ArbBrightnessConstraints
