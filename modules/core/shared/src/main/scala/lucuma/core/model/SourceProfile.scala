// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.math.BrightnessUnits._
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait SourceProfile extends Product with Serializable

object SourceProfile {

  final case class Point(spectralDefinition: SpectralDefinition[Integrated]) extends SourceProfile
  object Point {
    implicit val eqPoint: Eq[Point] = Eq.by(_.spectralDefinition)

    /** @group Optics */
    val spectralDefinition: Lens[Point, SpectralDefinition[Integrated]] =
      Focus[Point](_.spectralDefinition)
  }

  final case class Uniform(spectralDefinition: SpectralDefinition[Surface]) extends SourceProfile
  object Uniform {
    implicit val eqUniform: Eq[Uniform] = Eq.by(_.spectralDefinition)

    /** @group Optics */
    val spectralDefinition: Lens[Uniform, SpectralDefinition[Surface]] =
      Focus[Uniform](_.spectralDefinition)
  }

  /**
   * Gaussian source. For a good discussion of seeing see the ["Astronomical seeing" wikipedia
   * entry](https://en.wikipedia.org/wiki/Astronomical_seeing).
   *
   * @param fwhm
   *   full width at half maximum of the seeing disc (typically in arcsec)
   */
  final case class Gaussian(
    source:             GaussianSource,
    spectralDefinition: SpectralDefinition[Integrated]
  ) extends SourceProfile
  object Gaussian {

    implicit val eqGaussian: Eq[Gaussian] = Eq.by(x => (x.source, x.spectralDefinition))

    /** @group Optics */
    val source: Lens[Gaussian, GaussianSource] =
      Focus[Gaussian](_.source)

    /** @group Optics */
    val spectralDefinition: Lens[Gaussian, SpectralDefinition[Integrated]] =
      Focus[Gaussian](_.spectralDefinition)
  }

  implicit val eqSourceProfile: Eq[SourceProfile] = Eq.instance {
    case (a @ Point(_), b @ Point(_))             => a === b
    case (a @ Uniform(_), b @ Uniform(_))         => a === b
    case (a @ Gaussian(_, _), b @ Gaussian(_, _)) => a === b
    case _                                        => false
  }

  /** @group Optics */
  val point: Prism[SourceProfile, Point] = GenPrism[SourceProfile, Point]

  /** @group Optics */
  val uniform: Prism[SourceProfile, Uniform] = GenPrism[SourceProfile, Uniform]

  /** @group Optics */
  val gaussian: Prism[SourceProfile, Gaussian] = GenPrism[SourceProfile, Gaussian]

  /** @group Optics */
  val integratedSpectralDefinition: Optional[SourceProfile, SpectralDefinition[Integrated]] =
    Optional[SourceProfile, SpectralDefinition[Integrated]](p =>
      point
        .andThen(Point.spectralDefinition)
        .getOption(p)
        .orElse(
          gaussian
            .andThen(Gaussian.spectralDefinition)
            .getOption(p)
        )
    )(v => {
      case p @ Point(_)       => Point.spectralDefinition.replace(v)(p)
      case p @ Gaussian(_, _) => Gaussian.spectralDefinition.replace(v)(p)
      case p                  => p
    })

  /** @group Optics */
  val surfaceSpectralDefinition: Optional[SourceProfile, SpectralDefinition[Surface]] =
    uniform.andThen(Uniform.spectralDefinition)

  /** @group Optics */
  val gaussianSource: Optional[SourceProfile, GaussianSource] =
    gaussian.andThen(Gaussian.source)
}
