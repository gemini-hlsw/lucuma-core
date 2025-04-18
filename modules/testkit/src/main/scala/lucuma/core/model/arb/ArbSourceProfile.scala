// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.arb.ArbAngle
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbSourceProfile {
  import ArbAngle.given
  import ArbEnumerated.given
  import BrightnessUnits.*
  import ArbSpectralDefinition.given

  given Arbitrary[SourceProfile.Point] =
    Arbitrary(
      arbitrary[SpectralDefinition[Integrated]].map(SourceProfile.Point(_))
    )

  given Arbitrary[SourceProfile.Uniform] =
    Arbitrary(
      arbitrary[SpectralDefinition[Surface]].map(SourceProfile.Uniform(_))
    )

  given Arbitrary[SourceProfile.Gaussian] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
        d <- arbitrary[SpectralDefinition[Integrated]]
      } yield SourceProfile.Gaussian(a, d)
    }

  given Arbitrary[SourceProfile] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[SourceProfile.Point],
        arbitrary[SourceProfile.Uniform],
        arbitrary[SourceProfile.Gaussian]
      )
    }

  given Cogen[SourceProfile.Point] =
    Cogen[SpectralDefinition[Integrated]].contramap(_.spectralDefinition)

  given Cogen[SourceProfile.Uniform] =
    Cogen[SpectralDefinition[Surface]].contramap(_.spectralDefinition)

  given Cogen[SourceProfile.Gaussian] =
    Cogen[(Angle, SpectralDefinition[Integrated])].contramap(x => (x.fwhm, x.spectralDefinition))

  given Cogen[SourceProfile] =
    Cogen[
      Either[SourceProfile.Point, Either[SourceProfile.Uniform, SourceProfile.Gaussian]]
    ]
      .contramap {
        case p @ SourceProfile.Point(_)       => p.asLeft
        case p @ SourceProfile.Uniform(_)     => p.asLeft.asRight
        case p @ SourceProfile.Gaussian(_, _) => p.asRight.asRight
      }
}

object ArbSourceProfile extends ArbSourceProfile
