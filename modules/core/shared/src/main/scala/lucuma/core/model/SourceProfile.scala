// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional._
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.Traversal
import monocle.macros.GenPrism

import scala.collection.immutable.SortedMap

sealed trait SourceProfile extends Product with Serializable {

  /**
   * Convert to a `Point` source profile. Units are converted to their `Integrated` versions, if
   * necessary.
   */
  def toPoint: SourceProfile.Point

  /**
   * Convert to a `Uniform` source profile. Units are converted to their `Surface` versions, if
   * necessary.
   */
  def toUniform: SourceProfile.Uniform

  /**
   * Convert to a `Gaussian` source profile. Units are converted to their `Integrated` versions, if
   * necessary.
   */
  def toGaussian: SourceProfile.Gaussian
}

object SourceProfile {

  final case class Point(spectralDefinition: SpectralDefinition[Integrated]) extends SourceProfile {

    override def toPoint: Point = this

    override def toUniform: Uniform =
      Uniform(spectralDefinition.to[Surface])

    override def toGaussian: Gaussian = Gaussian(GaussianSource.Zero, spectralDefinition)
  }

  object Point {
    implicit val eqPoint: Eq[Point] = Eq.by(_.spectralDefinition)

    /** @group Optics */
    val spectralDefinition: Lens[Point, SpectralDefinition[Integrated]] =
      Focus[Point](_.spectralDefinition)

  }

  final case class Uniform(spectralDefinition: SpectralDefinition[Surface]) extends SourceProfile {

    override def toPoint: Point = Point(spectralDefinition.to[Integrated])

    override def toUniform: Uniform = this

    override def toGaussian: Gaussian =
      Gaussian(GaussianSource.Zero, spectralDefinition.to[Integrated])

  }
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
  ) extends SourceProfile {

    override def toPoint: Point = Point(spectralDefinition)

    override def toUniform: Uniform = Uniform(spectralDefinition.to[Surface])

    override def toGaussian: Gaussian = this

  }
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

  /** @group Optics */
  val integratedBandNormalizedSpectralDefinition
    : Optional[SourceProfile, SpectralDefinition.BandNormalized[Integrated]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.bandNormalized[Integrated])

  /** @group Optics */
  val surfaceBandNormalizedSpectralDefinition
    : Optional[SourceProfile, SpectralDefinition.BandNormalized[Surface]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.bandNormalized[Surface])

  /** @group Optics */
  val integratedEmissionLinesSpectralDefinition
    : Optional[SourceProfile, SpectralDefinition.EmissionLines[Integrated]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.emissionLines[Integrated])

  /** @group Optics */
  val surfaceEmissionLinesSpectralDefinition
    : Optional[SourceProfile, SpectralDefinition.EmissionLines[Surface]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.emissionLines[Surface])

  /** @group Optics */
  val unnormalizedSED: Optional[SourceProfile, UnnormalizedSED] =
    Optional[SourceProfile, UnnormalizedSED](p =>
      integratedSpectralDefinition
        .andThen(SpectralDefinition.unnormalizedSED[Integrated])
        .getOption(p)
        .orElse(
          surfaceSpectralDefinition
            .andThen(SpectralDefinition.unnormalizedSED[Surface])
            .getOption(p)
        )
    )(v => {
      case p @ Uniform(_) =>
        surfaceSpectralDefinition.andThen(SpectralDefinition.unnormalizedSED[Surface]).replace(v)(p)
      case p              =>
        integratedSpectralDefinition
          .andThen(SpectralDefinition.unnormalizedSED[Integrated])
          .replace(v)(p)
    })

  /** @group Optics */
  val integratedBandBrightnesses
    : Optional[SourceProfile, SortedMap[Band, BandBrightness[Integrated]]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.bandBrightnesses[Integrated])

  /** @group Optics */
  val surfaceBandBrightnesses: Optional[SourceProfile, SortedMap[Band, BandBrightness[Surface]]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.bandBrightnesses[Surface])

  /** @group Optics */
  val integratedBandBrightnessesT: Traversal[SourceProfile, BandBrightness[Integrated]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.bandBrightnessesT[Integrated])

  /** @group Optics */
  val surfaceBandBrightnessesT: Traversal[SourceProfile, BandBrightness[Surface]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.bandBrightnessesT[Surface])

  /** @group Optics */
  def integratedBandBrightnessIn[T](b: Band): Traversal[SourceProfile, BandBrightness[Integrated]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.bandBrightnessIn[Integrated](b))

  /** @group Optics */
  def surfaceBandBrightnessIn[T](b: Band): Traversal[SourceProfile, BandBrightness[Surface]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.bandBrightnessIn[Surface](b))

  /** @group Optics */
  val integratedWavelengthLines
    : Optional[SourceProfile, SortedMap[Wavelength, EmissionLine[Integrated]]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.wavelengthLines[Integrated])

  /** @group Optics */
  val surfaceWavelengthLines
    : Optional[SourceProfile, SortedMap[Wavelength, EmissionLine[Surface]]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.wavelengthLines[Surface])

  /** @group Optics */
  val integratedWavelengthLinesT: Traversal[SourceProfile, EmissionLine[Integrated]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.wavelengthLinesT[Integrated])

  /** @group Optics */
  val surfaceWavelengthLinesT: Traversal[SourceProfile, EmissionLine[Surface]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.wavelengthLinesT[Surface])

  /** @group Optics */
  def integratedWavelengthLineIn(
    w: Wavelength
  ): Traversal[SourceProfile, EmissionLine[Integrated]] =
    integratedSpectralDefinition.andThen(SpectralDefinition.wavelengthLineIn[Integrated](w))

  /** @group Optics */
  def surfaceWavelengthLineIn[T](w: Wavelength): Traversal[SourceProfile, EmissionLine[Surface]] =
    surfaceSpectralDefinition.andThen(SpectralDefinition.wavelengthLineIn[Surface](w))

  /** @group Optics */
  val integratedFluxDensityContinuum: Optional[
    SourceProfile,
    Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]
  ] = integratedSpectralDefinition.andThen(SpectralDefinition.fluxDensityContinuum[Integrated])

  /** @group Optics */
  val surfaceFluxDensityContinuum: Optional[
    SourceProfile,
    Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]
  ] = surfaceSpectralDefinition.andThen(SpectralDefinition.fluxDensityContinuum[Surface])
}
