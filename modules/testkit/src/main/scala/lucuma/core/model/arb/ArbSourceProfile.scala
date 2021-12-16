// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.all._
import lucuma.core.math.BrightnessUnits
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbSourceProfile {
  import ArbEnumerated._
  import ArbGaussianSource._
  import BrightnessUnits._
  import ArbSpectralDefinition._

  implicit val arbPointSourceProfile: Arbitrary[SourceProfile.Point] =
    Arbitrary(
      arbitrary[SpectralDefinition[Integrated]].map(SourceProfile.Point(_))
    )

  implicit val arbUniformSourceProfile: Arbitrary[SourceProfile.Uniform] =
    Arbitrary(
      arbitrary[SpectralDefinition[Surface]].map(SourceProfile.Uniform(_))
    )

  implicit val arbGaussianSourceProfile: Arbitrary[SourceProfile.Gaussian] =
    Arbitrary {
      for {
        g <- arbitrary[GaussianSource]
        d <- arbitrary[SpectralDefinition[Integrated]]
      } yield SourceProfile.Gaussian(g, d)
    }

  implicit val arbSourceProfile: Arbitrary[SourceProfile] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[SourceProfile.Point],
        arbitrary[SourceProfile.Uniform],
        arbitrary[SourceProfile.Gaussian]
      )
    }

  implicit val cogPointSourceProfile: Cogen[SourceProfile.Point] =
    Cogen[SpectralDefinition[Integrated]].contramap(_.spectralDefinition)

  implicit val cogUniformSourceProfile: Cogen[SourceProfile.Uniform] =
    Cogen[SpectralDefinition[Surface]].contramap(_.spectralDefinition)

  implicit val cogenGaussianSourceProfile: Cogen[SourceProfile.Gaussian] =
    Cogen[(GaussianSource, SpectralDefinition[Integrated])].contramap(x =>
      (x.source, x.spectralDefinition)
    )

  implicit val cogSourceProfile: Cogen[SourceProfile] =
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
