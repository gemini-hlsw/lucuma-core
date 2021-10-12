// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.implicits._
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbSpatialProfile {

  import ArbAngle._
  import SpatialProfile._

  implicit val arbGaussianSource: Arbitrary[GaussianSource] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
      } yield GaussianSource(a)
    }

  implicit val cogGaussianSource: Cogen[GaussianSource] =
    Cogen[Angle].contramap(_.fwhm)

  implicit val arbSpatialProfile: Arbitrary[SpatialProfile] =
    Arbitrary {
      for {
        g <- arbitrary[GaussianSource]
        r <- Gen.oneOf(g, PointSource, UniformSource)
      } yield r
    }

  implicit val cogSpatialProfile: Cogen[SpatialProfile] =
    Cogen[Either[Unit, Either[Unit, Angle]]].contramap {
      case PointSource       => ().asLeft
      case UniformSource     => ().asLeft.asRight
      case GaussianSource(a) => a.asRight.asRight
    }
}

object ArbSpatialProfile extends ArbSpatialProfile
