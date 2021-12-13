// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnit._

import scala.collection.immutable.SortedMap
import monocle.Lens
import monocle.Focus
import monocle.Prism
import monocle.macros.GenPrism
import monocle.Optional
import monocle.Traversal

sealed trait BrightnessProfile extends Product with Serializable {
  def bands: List[Band]
}

final case class PointBrightnessProfile(
  brightnesses: SortedMap[Band, TargetBrightness[Integrated]],
  sed:          SpectralDistribution[Integrated]
) extends BrightnessProfile {
  lazy val bands: List[Band] = brightnesses.keys.toList
}

object PointBrightnessProfile {
  implicit val eq: Eq[PointBrightnessProfile] = Eq.by(x => (x.brightnesses, x.sed))

  /** @group Optics */
  val brightnesses: Lens[PointBrightnessProfile, SortedMap[Band, TargetBrightness[Integrated]]] =
    Focus[PointBrightnessProfile](_.brightnesses)

  /** @group Optics */
  val brightnessesT: Traversal[PointBrightnessProfile, TargetBrightness[Integrated]] =
    brightnesses.each

  /** @group Optics */
  def brightnessIn(b: Band): Traversal[PointBrightnessProfile, TargetBrightness[Integrated]] =
    brightnesses.filterIndex((a: Band) => a === b)

  /** @group Optics */
  val sed: Lens[PointBrightnessProfile, SpectralDistribution[Integrated]] =
    Focus[PointBrightnessProfile](_.sed)
}

final case class UniformBrightnessProfile(
  brightnesses: SortedMap[Band, TargetBrightness[Surface]],
  sed:          SpectralDistribution[Surface]
) extends BrightnessProfile {
  lazy val bands: List[Band] = brightnesses.keys.toList
}

object UniformBrightnessProfile {
  implicit val eq: Eq[UniformBrightnessProfile] = Eq.by(x => (x.brightnesses, x.sed))

  /** @group Optics */
  val brightnesses: Lens[UniformBrightnessProfile, SortedMap[Band, TargetBrightness[Surface]]] =
    Focus[UniformBrightnessProfile](_.brightnesses)

  /** @group Optics */
  val brightnessesT: Traversal[UniformBrightnessProfile, TargetBrightness[Surface]] =
    brightnesses.each

  /** @group Optics */
  def brightnessIn(b: Band): Traversal[UniformBrightnessProfile, TargetBrightness[Surface]] =
    brightnesses.filterIndex((a: Band) => a === b)

  /** @group Optics */
  val sed: Lens[UniformBrightnessProfile, SpectralDistribution[Surface]] =
    Focus[UniformBrightnessProfile](_.sed)
}

/**
 * Gaussian source. For a good discussion of seeing see the ["Astronomical seeing" wikipedia
 * entry](https://en.wikipedia.org/wiki/Astronomical_seeing).
 *
 * @param fwhm
 *   full width at half maximum of the seeing disc (typically in arcsec)
 */
final case class GaussianBrightnessProfile(
  source:       GaussianSource,
  brightnesses: SortedMap[Band, TargetBrightness[Integrated]],
  sed:          SpectralDistribution[Integrated]
) extends BrightnessProfile {
  lazy val bands: List[Band] = brightnesses.keys.toList
}

object GaussianBrightnessProfile {
  implicit val eq: Eq[GaussianBrightnessProfile] = Eq.by(x => (x.source, x.brightnesses, x.sed))

  /** @group Optics */
  val source: Lens[GaussianBrightnessProfile, GaussianSource] =
    Focus[GaussianBrightnessProfile](_.source)

  /** @group Optics */
  val brightnesses: Lens[GaussianBrightnessProfile, SortedMap[Band, TargetBrightness[Integrated]]] =
    Focus[GaussianBrightnessProfile](_.brightnesses)

  /** @group Optics */
  val brightnessesT: Traversal[GaussianBrightnessProfile, TargetBrightness[Integrated]] =
    brightnesses.each

  /** @group Optics */
  def brightnessIn(b: Band): Traversal[GaussianBrightnessProfile, TargetBrightness[Integrated]] =
    brightnesses.filterIndex((a: Band) => a === b)

  /** @group Optics */
  val sed: Lens[GaussianBrightnessProfile, SpectralDistribution[Integrated]] =
    Focus[GaussianBrightnessProfile](_.sed)
}

object BrightnessProfile {
  implicit val eqBrightnessProfile: Eq[BrightnessProfile] = Eq.instance {
    case (a @ PointBrightnessProfile(_, _), b @ PointBrightnessProfile(_, _))             => a === b
    case (a @ UniformBrightnessProfile(_, _), b @ UniformBrightnessProfile(_, _))         => a === b
    case (a @ GaussianBrightnessProfile(_, _, _), b @ GaussianBrightnessProfile(_, _, _)) => a === b
    case _                                                                                => false
  }

  /** @group Optics */
  val point: Prism[BrightnessProfile, PointBrightnessProfile] =
    GenPrism[BrightnessProfile, PointBrightnessProfile]

  /** @group Optics */
  val uniform: Prism[BrightnessProfile, UniformBrightnessProfile] =
    GenPrism[BrightnessProfile, UniformBrightnessProfile]

  /** @group Optics */
  val gaussian: Prism[BrightnessProfile, GaussianBrightnessProfile] =
    GenPrism[BrightnessProfile, GaussianBrightnessProfile]

  /** @group Optics */
  val integratedBrightnesses
    : Optional[BrightnessProfile, SortedMap[Band, TargetBrightness[Integrated]]] =
    Optional[BrightnessProfile, SortedMap[Band, TargetBrightness[Integrated]]](p =>
      point
        .andThen(PointBrightnessProfile.brightnesses)
        .getOption(p)
        .orElse(
          gaussian
            .andThen(GaussianBrightnessProfile.brightnesses)
            .getOption(p)
        )
    )(v => {
      case p @ PointBrightnessProfile(_, _)       =>
        PointBrightnessProfile.brightnesses.replace(v)(p)
      case p @ GaussianBrightnessProfile(_, _, _) =>
        GaussianBrightnessProfile.brightnesses.replace(v)(p)
      case p                                      => p
    })

  /** @group Optics */
  val surfaceBrightnesses: Optional[BrightnessProfile, SortedMap[Band, TargetBrightness[Surface]]] =
    uniform.andThen(UniformBrightnessProfile.brightnesses)

  /** @group Optics */
  val integratedBrightnessesT: Traversal[BrightnessProfile, TargetBrightness[Integrated]] =
    integratedBrightnesses.each

  /** @group Optics */
  def integratedBrightnessIn(b: Band): Traversal[BrightnessProfile, TargetBrightness[Integrated]] =
    integratedBrightnesses.filterIndex((a: Band) => a === b)

  /** @group Optics */
  val surfaceBrightnessesT: Traversal[BrightnessProfile, TargetBrightness[Surface]] =
    surfaceBrightnesses.each

  /** @group Optics */
  def surfaceBrightnessIn(b: Band): Traversal[BrightnessProfile, TargetBrightness[Surface]] =
    surfaceBrightnesses.filterIndex((a: Band) => a === b)

  /** @group Optics */
  val integratedSED: Optional[BrightnessProfile, SpectralDistribution[Integrated]] =
    Optional[BrightnessProfile, SpectralDistribution[Integrated]](p =>
      point
        .andThen(PointBrightnessProfile.sed)
        .getOption(p)
        .orElse(
          gaussian
            .andThen(GaussianBrightnessProfile.sed)
            .getOption(p)
        )
    )(v => {
      case p @ PointBrightnessProfile(_, _)       =>
        PointBrightnessProfile.sed.replace(v)(p)
      case p @ GaussianBrightnessProfile(_, _, _) =>
        GaussianBrightnessProfile.sed.replace(v)(p)
      case p                                      => p
    })

  /** @group Optics */
  val surfaceSED: Optional[BrightnessProfile, SpectralDistribution[Surface]] =
    uniform.andThen(UniformBrightnessProfile.sed)

  /** @group Optics */
  val gaussianSource: Optional[BrightnessProfile, GaussianSource] =
    gaussian.andThen(GaussianBrightnessProfile.source)

}
