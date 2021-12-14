// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.all._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.dimensional._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbBrightnessProfile {
  import ArbSpectralDistribution._
  import ArbTargetBrightness._
  import ArbEnumerated._
  import ArbGaussianSource._
  import BrightnessUnits._

  implicit def arbBrightnessesMap[T](implicit
    arbUnit: Arbitrary[GroupedUnitType[Brightness[T]]]
  ): Arbitrary[SortedMap[Band, TargetBrightness[T]]] =
    Arbitrary(
      arbitrary[Vector[TargetBrightness[T]]].map(_.fproductLeft(_.band)).map(x => SortedMap(x: _*))
    )

  implicit def cogBrightnessesMap[T]: Cogen[SortedMap[Band, TargetBrightness[T]]] =
    Cogen[Vector[(Band, TargetBrightness[T])]].contramap(_.toVector)

  implicit val arbPointBrightnessProfile: Arbitrary[PointBrightnessProfile] =
    Arbitrary {
      for {
        b <- arbitrary[Vector[(Band, TargetBrightness[Integrated])]]
        s <- arbitrary[SpectralDistribution[Integrated]]
      } yield PointBrightnessProfile(SortedMap(b: _*), s)
    }

  implicit val arbUniformBrightnessProfile: Arbitrary[UniformBrightnessProfile] =
    Arbitrary {
      for {
        b <- arbitrary[Vector[(Band, TargetBrightness[Surface])]]
        s <- arbitrary[SpectralDistribution[Surface]]
      } yield UniformBrightnessProfile(SortedMap(b: _*), s)
    }

  implicit val arbGaussianBrightnessProfile: Arbitrary[GaussianBrightnessProfile] =
    Arbitrary {
      for {
        g <- arbitrary[GaussianSource]
        b <- arbitrary[Vector[(Band, TargetBrightness[Integrated])]]
        s <- arbitrary[SpectralDistribution[Integrated]]
      } yield GaussianBrightnessProfile(g, SortedMap(b: _*), s)
    }

  implicit val arbBrightnessProfile: Arbitrary[BrightnessProfile] =
    Arbitrary {
      Gen.oneOf(
        arbitrary[PointBrightnessProfile],
        arbitrary[UniformBrightnessProfile],
        arbitrary[GaussianBrightnessProfile]
      )
    }

  implicit val cogenPointBrightnessProfile: Cogen[PointBrightnessProfile] =
    Cogen[(Map[Band, TargetBrightness[Integrated]], SpectralDistribution[Integrated])].contramap(
      x => (x.brightnesses, x.sed)
    )

  implicit val cogenUniformBrightnessProfile: Cogen[UniformBrightnessProfile] =
    Cogen[(Map[Band, TargetBrightness[Surface]], SpectralDistribution[Surface])].contramap(x =>
      (x.brightnesses, x.sed)
    )

  implicit val cogenGaussianBrightnessProfile: Cogen[GaussianBrightnessProfile] =
    Cogen[
      (GaussianSource, Map[Band, TargetBrightness[Integrated]], SpectralDistribution[Integrated])
    ].contramap(x => (x.source, x.brightnesses, x.sed))

  implicit val cogBrightnessProfile: Cogen[BrightnessProfile] =
    Cogen[
      Either[PointBrightnessProfile, Either[UniformBrightnessProfile, GaussianBrightnessProfile]]
    ]
      .contramap {
        case p @ PointBrightnessProfile(_, _)       => p.asLeft
        case p @ UniformBrightnessProfile(_, _)     => p.asLeft.asRight
        case p @ GaussianBrightnessProfile(_, _, _) => p.asRight.asRight
      }
}

object ArbBrightnessProfile extends ArbBrightnessProfile
