// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math._
import lucuma.core.math.dimensional._
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.Traversal
import monocle.macros.GenPrism

import scala.collection.immutable.SortedMap

/** A target of observation. */
sealed trait Target extends Product with Serializable {
  def name: NonEmptyString
  def sourceProfile: SourceProfile
  def angularSize: Option[AngularSize] // This is just used for visualization
}

object Target extends WithId('t') with TargetOptics {

  final case class Sidereal(
    name:          NonEmptyString,
    tracking:      SiderealTracking,
    sourceProfile: SourceProfile,
    catalogInfo:   Option[CatalogInfo],
    angularSize:   Option[AngularSize]
  ) extends Target

  object Sidereal extends SiderealOptics {
    implicit val eqSidereal: Eq[Sidereal] =
      Eq.by(x => (x.name, x.tracking, x.sourceProfile, x.catalogInfo))

    /**
     * A sidereal target order based on tracking information, which roughly means by base coordinate
     * without applying proper motion.
     *
     * Not implicit.
     */
    val TrackOrder: Order[Sidereal] = Order.by(x => (x.tracking, x.name))

    /**
     * Sidereal targets ordered by name first and then tracking information.
     *
     * Not implicit.
     */
    val NameOrder: Order[Sidereal] = Order.by(x => (x.name, x.tracking))
  }

  final case class Nonsidereal(
    name:          NonEmptyString,
    ephemerisKey:  EphemerisKey,
    sourceProfile: SourceProfile,
    angularSize:   Option[AngularSize]
  ) extends Target

  object Nonsidereal extends NonsiderealOptics {
    implicit val eqNonsidereal: Eq[Nonsidereal] =
      Eq.by(x => (x.name, x.ephemerisKey, x.sourceProfile))

    /**
     * A nonsidereal target order based on ephemeris key.
     *
     * Not implicit.
     */
    val TrackOrder: Order[Nonsidereal] = Order.by(x => (x.ephemerisKey, x.name))

    /**
     * Nonsidereal targets ordered by name first and then ephemeris key.
     *
     * Not implicit.
     */
    val NameOrder: Order[Nonsidereal] = Order.by(x => (x.name, x.ephemerisKey))
  }

  implicit val TargetEq: Eq[Target] = Eq.instance {
    case (a @ Sidereal(_, _, _, _, _), b @ Sidereal(_, _, _, _, _)) => a === b
    case (a @ Nonsidereal(_, _, _, _), b @ Nonsidereal(_, _, _, _)) => a === b
    case _                                                          => false
  }

  /**
   * A target order based on tracking information. For sidereal targets this roughly means by base
   * coordinate without applying proper motion. For non-sidereal this means by `EphemerisKey`.
   *
   * Not implicit.
   */
  val TrackOrder: Order[Target] =
    Order.from {
      case (a @ Sidereal(_, _, _, _, _), b @ Sidereal(_, _, _, _, _)) =>
        Sidereal.TrackOrder.compare(a, b)
      case (a @ Nonsidereal(_, _, _, _), b @ Nonsidereal(_, _, _, _)) =>
        Nonsidereal.TrackOrder.compare(a, b)
      case (Nonsidereal(_, _, _, _), _)                               => -1
      case _                                                          => 1
    }

  /**
   * Targets ordered by name first and then tracking information.
   *
   * Not implicit.
   */
  val NameOrder: Order[Target] =
    Order.from {
      case (a @ Sidereal(_, _, _, _, _), b @ Sidereal(_, _, _, _, _)) =>
        Sidereal.NameOrder.compare(a, b)
      case (a @ Nonsidereal(_, _, _, _), b @ Nonsidereal(_, _, _, _)) =>
        Nonsidereal.NameOrder.compare(a, b)
      case (Nonsidereal(_, _, _, _), _)                               => -1
      case _                                                          => 1
    }

  trait SiderealOptics { this: Sidereal.type =>

    /** @group Optics */
    val name: Lens[Sidereal, NonEmptyString] =
      Focus[Sidereal](_.name)

    /** @group Optics */
    val tracking: Lens[Sidereal, SiderealTracking] =
      Focus[Sidereal](_.tracking)

    /** @group Optics */
    val parallax: Lens[Sidereal, Option[Parallax]] =
      tracking.andThen(SiderealTracking.parallax)

    /** @group Optics */
    val radialVelocity: Lens[Sidereal, Option[RadialVelocity]] =
      tracking.andThen(SiderealTracking.radialVelocity)

    /** @group Optics */
    val baseCoordinates: Lens[Sidereal, Coordinates] =
      tracking.andThen(SiderealTracking.baseCoordinates)

    /** @group Optics */
    val baseRA: Lens[Sidereal, RightAscension] =
      baseCoordinates.andThen(Coordinates.rightAscension)

    /** @group Optics */
    val baseDec: Lens[Sidereal, Declination] =
      baseCoordinates.andThen(Coordinates.declination)

    /** @group Optics */
    val epoch: Lens[Sidereal, Epoch] =
      tracking.andThen(SiderealTracking.epoch)

    /** @group Optics */
    val properMotion: Lens[Sidereal, Option[ProperMotion]] =
      tracking.andThen(SiderealTracking.properMotion)

    /** @group Optics */
    val properMotionRA: Optional[Sidereal, ProperMotion.RA] =
      properMotion.some.andThen(ProperMotion.ra)

    /** @group Optics */
    val properMotionDec: Optional[Sidereal, ProperMotion.Dec] =
      properMotion.some.andThen(ProperMotion.dec)

    /** @group Optics */
    val sourceProfile: Lens[Sidereal, SourceProfile] =
      Focus[Sidereal](_.sourceProfile)

    /** @group Optics */
    val integratedSpectralDefinition: Optional[Sidereal, SpectralDefinition[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedSpectralDefinition)

    /** @group Optics */
    val surfaceSpectralDefinition: Optional[Sidereal, SpectralDefinition[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceSpectralDefinition)

    /** @group Optics */
    val gaussianSource: Optional[Sidereal, GaussianSource] =
      sourceProfile.andThen(SourceProfile.gaussianSource)

    /** @group Optics */
    val integratedBandNormalizedSpectralDefinition
      : Optional[Sidereal, SpectralDefinition.BandNormalized[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBandNormalizedSpectralDefinition)

    /** @group Optics */
    val surfaceBandNormalizedSpectralDefinition
      : Optional[Sidereal, SpectralDefinition.BandNormalized[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBandNormalizedSpectralDefinition)

    /** @group Optics */
    val integratedEmissionLinesSpectralDefinition
      : Optional[Sidereal, SpectralDefinition.EmissionLines[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedEmissionLinesSpectralDefinition)

    /** @group Optics */
    val surfaceEmissionLinesSpectralDefinition
      : Optional[Sidereal, SpectralDefinition.EmissionLines[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceEmissionLinesSpectralDefinition)

    /** @group Optics */
    val unnormalizedSED: Optional[Sidereal, UnnormalizedSED] =
      sourceProfile.andThen(SourceProfile.unnormalizedSED)

    /** @group Optics */
    val integratedBandBrightnesses
      : Optional[Sidereal, SortedMap[Band, BandBrightness[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedBandBrightnesses)

    /** @group Optics */
    val surfaceBandBrightnesses: Optional[Sidereal, SortedMap[Band, BandBrightness[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceBandBrightnesses)

    /** @group Optics */
    val integratedBandBrightnessesT: Traversal[Sidereal, BandBrightness[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBandBrightnessesT)

    /** @group Optics */
    val surfaceBandBrightnessesT: Traversal[Sidereal, BandBrightness[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBandBrightnessesT)

    /** @group Optics */
    def integratedBandBrightnessIn[T](
      b: Band
    ): Traversal[Sidereal, BandBrightness[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBandBrightnessIn(b))

    /** @group Optics */
    def surfaceBandBrightnessIn[T](b: Band): Traversal[Sidereal, BandBrightness[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBandBrightnessIn(b))

    /** @group Optics */
    val integratedWavelengthLines
      : Optional[Sidereal, SortedMap[Wavelength, EmissionLine[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLines)

    /** @group Optics */
    val surfaceWavelengthLines: Optional[Sidereal, SortedMap[Wavelength, EmissionLine[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLines)

    /** @group Optics */
    val integratedWavelengthLinesT: Traversal[Sidereal, EmissionLine[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLinesT)

    /** @group Optics */
    val surfaceWavelengthLinesT: Traversal[Sidereal, EmissionLine[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLinesT)

    /** @group Optics */
    def integratedWavelengthLineIn(
      w: Wavelength
    ): Traversal[Sidereal, EmissionLine[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLineIn(w))

    /** @group Optics */
    def surfaceWavelengthLineIn[T](w: Wavelength): Traversal[Sidereal, EmissionLine[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLineIn(w))

    /** @group Optics */
    val integratedFluxDensityContinuum: Optional[
      Sidereal,
      Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]
    ] = sourceProfile.andThen(SourceProfile.integratedFluxDensityContinuum)

    /** @group Optics */
    val surfaceFluxDensityContinuum: Optional[
      Sidereal,
      Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]
    ] = sourceProfile.andThen(SourceProfile.surfaceFluxDensityContinuum)

    /** @group Optics */
    val catalogInfo: Lens[Sidereal, Option[CatalogInfo]] =
      Focus[Sidereal](_.catalogInfo)

    /** @group Optics */
    val angularSize: Lens[Sidereal, Option[AngularSize]] =
      Focus[Sidereal](_.angularSize)
  }

  trait NonsiderealOptics { this: Nonsidereal.type =>

    /** @group Optics */
    val name: Lens[Nonsidereal, NonEmptyString] =
      Focus[Nonsidereal](_.name)

    /** @group Optics */
    val ephemerisKey: Lens[Nonsidereal, EphemerisKey] =
      Focus[Nonsidereal](_.ephemerisKey)

    val sourceProfile: Lens[Nonsidereal, SourceProfile] =
      Focus[Nonsidereal](_.sourceProfile)

    /** @group Optics */
    val integratedSpectralDefinition: Optional[Nonsidereal, SpectralDefinition[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedSpectralDefinition)

    /** @group Optics */
    val surfaceSpectralDefinition: Optional[Nonsidereal, SpectralDefinition[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceSpectralDefinition)

    /** @group Optics */
    val gaussianSource: Optional[Nonsidereal, GaussianSource] =
      sourceProfile.andThen(SourceProfile.gaussianSource)

    /** @group Optics */
    val integratedBandNormalizedSpectralDefinition
      : Optional[Nonsidereal, SpectralDefinition.BandNormalized[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBandNormalizedSpectralDefinition)

    /** @group Optics */
    val surfaceBandNormalizedSpectralDefinition
      : Optional[Nonsidereal, SpectralDefinition.BandNormalized[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBandNormalizedSpectralDefinition)

    /** @group Optics */
    val integratedEmissionLinesSpectralDefinition
      : Optional[Nonsidereal, SpectralDefinition.EmissionLines[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedEmissionLinesSpectralDefinition)

    /** @group Optics */
    val surfaceEmissionLinesSpectralDefinition
      : Optional[Nonsidereal, SpectralDefinition.EmissionLines[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceEmissionLinesSpectralDefinition)

    /** @group Optics */
    val unnormalizedSED: Optional[Nonsidereal, UnnormalizedSED] =
      sourceProfile.andThen(SourceProfile.unnormalizedSED)

    /** @group Optics */
    val integratedBandBrightnesses
      : Optional[Nonsidereal, SortedMap[Band, BandBrightness[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedBandBrightnesses)

    /** @group Optics */
    val surfaceBandBrightnesses: Optional[Nonsidereal, SortedMap[Band, BandBrightness[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceBandBrightnesses)

    /** @group Optics */
    val integratedBandBrightnessesT: Traversal[Nonsidereal, BandBrightness[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBandBrightnessesT)

    /** @group Optics */
    val surfaceBandBrightnessesT: Traversal[Nonsidereal, BandBrightness[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBandBrightnessesT)

    /** @group Optics */
    def integratedBandBrightnessIn[T](
      b: Band
    ): Traversal[Nonsidereal, BandBrightness[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBandBrightnessIn(b))

    /** @group Optics */
    def surfaceBandBrightnessIn[T](b: Band): Traversal[Nonsidereal, BandBrightness[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBandBrightnessIn(b))

    /** @group Optics */
    val integratedWavelengthLines
      : Optional[Nonsidereal, SortedMap[Wavelength, EmissionLine[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLines)

    /** @group Optics */
    val surfaceWavelengthLines
      : Optional[Nonsidereal, SortedMap[Wavelength, EmissionLine[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLines)

    /** @group Optics */
    val integratedWavelengthLinesT: Traversal[Nonsidereal, EmissionLine[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLinesT)

    /** @group Optics */
    val surfaceWavelengthLinesT: Traversal[Nonsidereal, EmissionLine[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLinesT)

    /** @group Optics */
    def integratedWavelengthLineIn(
      w: Wavelength
    ): Traversal[Nonsidereal, EmissionLine[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLineIn(w))

    /** @group Optics */
    def surfaceWavelengthLineIn[T](w: Wavelength): Traversal[Nonsidereal, EmissionLine[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLineIn(w))

    /** @group Optics */
    val integratedFluxDensityContinuum: Optional[
      Nonsidereal,
      Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]
    ] = sourceProfile.andThen(SourceProfile.integratedFluxDensityContinuum)

    /** @group Optics */
    val surfaceFluxDensityContinuum: Optional[
      Nonsidereal,
      Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]
    ] = sourceProfile.andThen(SourceProfile.surfaceFluxDensityContinuum)

    /** @group Optics */
    val angularSize: Lens[Nonsidereal, Option[AngularSize]] =
      Focus[Nonsidereal](_.angularSize)
  }
}

trait TargetOptics { this: Target.type =>

  /** @group Optics */
  val sidereal: Prism[Target, Target.Sidereal] = GenPrism[Target, Target.Sidereal]

  /** @group Optics */
  val nonsidereal: Prism[Target, Target.Nonsidereal] = GenPrism[Target, Target.Nonsidereal]

  /** @group Optics */
  val name: Lens[Target, NonEmptyString] =
    Lens[Target, NonEmptyString](_.name)(v => {
      case t @ Target.Sidereal(_, _, _, _, _) => Target.Sidereal.name.replace(v)(t)
      case t @ Target.Nonsidereal(_, _, _, _) => Target.Nonsidereal.name.replace(v)(t)
    })

  /** @group Optics */
  val ephemerisKey: Optional[Target, EphemerisKey] =
    nonsidereal.andThen(Nonsidereal.ephemerisKey)

  /** @group Optics */
  val siderealTracking: Optional[Target, SiderealTracking] =
    sidereal.andThen(Sidereal.tracking)

  /** @group Optics */
  val sourceProfile: Lens[Target, SourceProfile] =
    Lens[Target, SourceProfile](_.sourceProfile)(v => {
      case t @ Target.Sidereal(_, _, _, _, _) => Target.Sidereal.sourceProfile.replace(v)(t)
      case t @ Target.Nonsidereal(_, _, _, _) => Target.Nonsidereal.sourceProfile.replace(v)(t)
    })

  /** @group Optics */
  val integratedSpectralDefinition: Optional[Target, SpectralDefinition[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedSpectralDefinition)

  /** @group Optics */
  val surfaceSpectralDefinition: Optional[Target, SpectralDefinition[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceSpectralDefinition)

  /** @group Optics */
  val gaussianSource: Optional[Target, GaussianSource] =
    sourceProfile.andThen(SourceProfile.gaussianSource)

  /** @group Optics */
  val integratedBandNormalizedSpectralDefinition
    : Optional[Target, SpectralDefinition.BandNormalized[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedBandNormalizedSpectralDefinition)

  /** @group Optics */
  val surfaceBandNormalizedSpectralDefinition
    : Optional[Target, SpectralDefinition.BandNormalized[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceBandNormalizedSpectralDefinition)

  /** @group Optics */
  val integratedEmissionLinesSpectralDefinition
    : Optional[Target, SpectralDefinition.EmissionLines[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedEmissionLinesSpectralDefinition)

  /** @group Optics */
  val surfaceEmissionLinesSpectralDefinition
    : Optional[Target, SpectralDefinition.EmissionLines[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceEmissionLinesSpectralDefinition)

  /** @group Optics */
  val unnormalizedSED: Optional[Target, UnnormalizedSED] =
    sourceProfile.andThen(SourceProfile.unnormalizedSED)

  /** @group Optics */
  val integratedBandBrightnesses: Optional[Target, SortedMap[Band, BandBrightness[Integrated]]] =
    sourceProfile.andThen(SourceProfile.integratedBandBrightnesses)

  /** @group Optics */
  val surfaceBandBrightnesses: Optional[Target, SortedMap[Band, BandBrightness[Surface]]] =
    sourceProfile.andThen(SourceProfile.surfaceBandBrightnesses)

  /** @group Optics */
  val integratedBandBrightnessesT: Traversal[Target, BandBrightness[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedBandBrightnessesT)

  /** @group Optics */
  val surfaceBandBrightnessesT: Traversal[Target, BandBrightness[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceBandBrightnessesT)

  /** @group Optics */
  def integratedBandBrightnessIn[T](
    b: Band
  ): Traversal[Target, BandBrightness[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedBandBrightnessIn(b))

  /** @group Optics */
  def surfaceBandBrightnessIn[T](b: Band): Traversal[Target, BandBrightness[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceBandBrightnessIn(b))

  /** @group Optics */
  val integratedWavelengthLines: Optional[Target, SortedMap[Wavelength, EmissionLine[Integrated]]] =
    sourceProfile.andThen(SourceProfile.integratedWavelengthLines)

  /** @group Optics */
  val surfaceWavelengthLines: Optional[Target, SortedMap[Wavelength, EmissionLine[Surface]]] =
    sourceProfile.andThen(SourceProfile.surfaceWavelengthLines)

  /** @group Optics */
  val integratedWavelengthLinesT: Traversal[Target, EmissionLine[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedWavelengthLinesT)

  /** @group Optics */
  val surfaceWavelengthLinesT: Traversal[Target, EmissionLine[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceWavelengthLinesT)

  /** @group Optics */
  def integratedWavelengthLineIn(
    w: Wavelength
  ): Traversal[Target, EmissionLine[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedWavelengthLineIn(w))

  /** @group Optics */
  def surfaceWavelengthLineIn[T](w: Wavelength): Traversal[Target, EmissionLine[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceWavelengthLineIn(w))

  /** @group Optics */
  val integratedFluxDensityContinuum: Optional[
    Target,
    Measure[PosBigDecimal] Of FluxDensityContinuum[Integrated]
  ] = sourceProfile.andThen(SourceProfile.integratedFluxDensityContinuum)

  /** @group Optics */
  val surfaceFluxDensityContinuum: Optional[
    Target,
    Measure[PosBigDecimal] Of FluxDensityContinuum[Surface]
  ] = sourceProfile.andThen(SourceProfile.surfaceFluxDensityContinuum)

  /** @group Optics */
  val parallax: Optional[Target, Option[Parallax]] =
    siderealTracking.andThen(SiderealTracking.parallax)

  /** @group Optics */
  val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    siderealTracking.andThen(SiderealTracking.radialVelocity)

  /** @group Optics */
  val baseCoordinates: Optional[Target, Coordinates] =
    siderealTracking.andThen(SiderealTracking.baseCoordinates)

  /** @group Optics */
  val baseRA: Optional[Target, RightAscension] =
    baseCoordinates.andThen(Coordinates.rightAscension)

  /** @group Optics */
  val baseDec: Optional[Target, Declination] =
    baseCoordinates.andThen(Coordinates.declination)

  /** @group Optics */
  val epoch: Optional[Target, Epoch] =
    sidereal.andThen(Sidereal.epoch)

  /** @group Optics */
  val properMotion: Optional[Target, Option[ProperMotion]] =
    sidereal.andThen(Sidereal.properMotion)

  /** @group Optics */
  val properMotionRA: Optional[Target, ProperMotion.RA] =
    sidereal.andThen(Sidereal.properMotionRA)

  /** @group Optics */
  val properMotionDec: Optional[Target, ProperMotion.Dec] =
    sidereal.andThen(Sidereal.properMotionDec)

  /** @group Optics */
  val catalogInfo: Optional[Target, Option[CatalogInfo]] =
    sidereal.andThen(Sidereal.catalogInfo)

  /** @group Optics */
  val angularSize: Lens[Target, Option[AngularSize]] =
    Lens[Target, Option[AngularSize]](_.angularSize)(v => {
      case t @ Target.Sidereal(_, _, _, _, _) => Target.Sidereal.angularSize.replace(v)(t)
      case t @ Target.Nonsidereal(_, _, _, _) => Target.Nonsidereal.angularSize.replace(v)(t)
    })
}
