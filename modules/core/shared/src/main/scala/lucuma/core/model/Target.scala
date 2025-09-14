// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import cats.implicits.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Band
import lucuma.core.math.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.refined.auto.*
import lucuma.core.util.*
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
}

object Target extends WithGid('t'.refined) with TargetOptics {

  case class Sidereal(
    name:          NonEmptyString,
    tracking:      SiderealTracking,
    sourceProfile: SourceProfile,
    catalogInfo:   Option[CatalogInfo]
  ) extends Target

  object Sidereal extends SiderealOptics {
    given Eq[Sidereal] =
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

  case class Nonsidereal(
    name:          NonEmptyString,
    ephemerisKey:  EphemerisKey,
    sourceProfile: SourceProfile
  ) extends Target

  object Nonsidereal extends NonsiderealOptics {
    given Eq[Nonsidereal] =
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

  case class Opportunity(
    name:          NonEmptyString,
    region:        Region,
    sourceProfile: SourceProfile
  ) extends Target

  object Opportunity extends OpportunityOptics {

    given Eq[Opportunity] =
      Eq.by(a => (a.name, a.region, a.sourceProfile))

    val NameOrder: Order[Opportunity] =
      Order.by(x => (x.name, x.region))

    val RegionOrder: Order[Opportunity] =
      Order.by(x => (x.region, x.name))

  }

  given Eq[Target] = Eq.instance {
    case (a @ Sidereal(_, _, _, _), b @ Sidereal(_, _, _, _)) => a === b
    case (a @ Nonsidereal(_, _, _), b @ Nonsidereal(_, _, _)) => a === b
    case (a @ Opportunity(_, _, _), b @ Opportunity(_, _, _)) => a === b
    case _                                                    => false
  }

  /**
   * A target order based on tracking information. For sidereal targets this roughly means by base
   * coordinate without applying proper motion. For non-sidereal this means by `EphemerisKey`.
   *
   * Not implicit.
   */
  val TrackOrder: Order[Target] =
    Order.from {
      case (a @ Sidereal(_, _, _, _), b @ Sidereal(_, _, _, _)) =>
        Sidereal.TrackOrder.compare(a, b)
      case (a @ Nonsidereal(_, _, _), b @ Nonsidereal(_, _, _)) =>
        Nonsidereal.TrackOrder.compare(a, b)
      case (a @ Opportunity(_, _, _), b @ Opportunity(_, _, _)) =>
        Opportunity.RegionOrder.compare(a, b)

      // Nonsidereal sorts first
      case (Nonsidereal(_, _, _), Sidereal(_, _, _, _)) => -1
      case (Nonsidereal(_, _, _), Opportunity(_, _, _)) => -1

      // Then Sidereal
      case (Sidereal(_, _, _, _), Nonsidereal(_, _, _)) => 1
      case (Sidereal(_, _, _, _), Opportunity(_, _, _)) => -1

      // Then Opportunity
      case (Opportunity(_, _, _), Sidereal(_, _, _, _)) => 1
      case (Opportunity(_, _, _), Nonsidereal(_, _, _)) => 1

    }

  /**
   * Targets ordered by name first and then tracking information.
   *
   * Not implicit.
   */
  val NameOrder: Order[Target] =
    Order.from {
      case (a @ Sidereal(_, _, _, _), b @ Sidereal(_, _, _, _)) =>
        Sidereal.NameOrder.compare(a, b)
      case (a @ Nonsidereal(_, _, _), b @ Nonsidereal(_, _, _)) =>
        Nonsidereal.NameOrder.compare(a, b)
      case (a @ Opportunity(_, _, _), b @ Opportunity(_, _, _)) =>
        Opportunity.NameOrder.compare(a, b)

      // Nonsidereal sorts first
      case (Nonsidereal(_, _, _), Sidereal(_, _, _, _)) => -1
      case (Nonsidereal(_, _, _), Opportunity(_, _, _)) => -1

      // Then Sidereal
      case (Sidereal(_, _, _, _), Nonsidereal(_, _, _)) => 1
      case (Sidereal(_, _, _, _), Opportunity(_, _, _)) => -1

      // Then Opportunity
      case (Opportunity(_, _, _), Sidereal(_, _, _, _)) => 1
      case (Opportunity(_, _, _), Nonsidereal(_, _, _)) => 1

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
      tracking.andThen(SiderealTracking.baseRa)

    /** @group Optics */
    val baseDec: Lens[Sidereal, Declination] =
      tracking.andThen(SiderealTracking.baseDec)

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
    val fwhm: Optional[Sidereal, Angle] =
      sourceProfile.andThen(SourceProfile.fwhm)

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
    val unnormalizedSED: Optional[Sidereal, Option[UnnormalizedSED]] =
      sourceProfile.andThen(SourceProfile.unnormalizedSED)

    /** @group Optics */
    val integratedBrightnesses: Optional[Sidereal, SortedMap[Band, BrightnessMeasure[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnesses)

    /** @group Optics */
    val surfaceBrightnesses: Optional[Sidereal, SortedMap[Band, BrightnessMeasure[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnesses)

    /** @group Optics */
    val integratedBrightnessesT: Traversal[Sidereal, BrightnessMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnessesT)

    /** @group Optics */
    val surfaceBrightnessesT: Traversal[Sidereal, BrightnessMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnessesT)

    /** @group Optics */
    def integratedBrightnessIn[T](
      b: Band
    ): Traversal[Sidereal, BrightnessMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnessIn(b))

    /** @group Optics */
    def surfaceBrightnessIn[T](b: Band): Traversal[Sidereal, BrightnessMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnessIn(b))

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
    val integratedFluxDensityContinuum: Optional[Sidereal, FluxDensityContinuumMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedFluxDensityContinuum)

    /** @group Optics */
    val surfaceFluxDensityContinuum: Optional[Sidereal, FluxDensityContinuumMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceFluxDensityContinuum)

    /** @group Optics */
    val catalogInfo: Lens[Sidereal, Option[CatalogInfo]] =
      Focus[Sidereal](_.catalogInfo)
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
    val fwhm: Optional[Nonsidereal, Angle] =
      sourceProfile.andThen(SourceProfile.fwhm)

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
    val unnormalizedSED: Optional[Nonsidereal, Option[UnnormalizedSED]] =
      sourceProfile.andThen(SourceProfile.unnormalizedSED)

    /** @group Optics */
    val integratedBrightnesses
      : Optional[Nonsidereal, SortedMap[Band, BrightnessMeasure[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnesses)

    /** @group Optics */
    val surfaceBrightnesses: Optional[Nonsidereal, SortedMap[Band, BrightnessMeasure[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnesses)

    /** @group Optics */
    val integratedBrightnessesT: Traversal[Nonsidereal, BrightnessMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnessesT)

    /** @group Optics */
    val surfaceBrightnessesT: Traversal[Nonsidereal, BrightnessMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnessesT)

    /** @group Optics */
    def integratedBrightnessIn[T](
      b: Band
    ): Traversal[Nonsidereal, BrightnessMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnessIn(b))

    /** @group Optics */
    def surfaceBrightnessIn[T](b: Band): Traversal[Nonsidereal, BrightnessMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnessIn(b))

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
    val integratedFluxDensityContinuum: Optional[Nonsidereal, FluxDensityContinuumMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedFluxDensityContinuum)

    /** @group Optics */
    val surfaceFluxDensityContinuum: Optional[Nonsidereal, FluxDensityContinuumMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceFluxDensityContinuum)
  }

  trait OpportunityOptics { this: Opportunity.type =>

    /** @group Optics */
    val name: Lens[Opportunity, NonEmptyString] =
      Focus[Opportunity](_.name)

    /** @group Optics */
    val region: Lens[Opportunity, Region] =
      Focus[Opportunity](_.region)

    val sourceProfile: Lens[Opportunity, SourceProfile] =
      Focus[Opportunity](_.sourceProfile)

    /** @group Optics */
    val integratedSpectralDefinition: Optional[Opportunity, SpectralDefinition[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedSpectralDefinition)

    /** @group Optics */
    val surfaceSpectralDefinition: Optional[Opportunity, SpectralDefinition[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceSpectralDefinition)

    /** @group Optics */
    val fwhm: Optional[Opportunity, Angle] =
      sourceProfile.andThen(SourceProfile.fwhm)

    /** @group Optics */
    val integratedBandNormalizedSpectralDefinition
      : Optional[Opportunity, SpectralDefinition.BandNormalized[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBandNormalizedSpectralDefinition)

    /** @group Optics */
    val surfaceBandNormalizedSpectralDefinition
      : Optional[Opportunity, SpectralDefinition.BandNormalized[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBandNormalizedSpectralDefinition)

    /** @group Optics */
    val integratedEmissionLinesSpectralDefinition
      : Optional[Opportunity, SpectralDefinition.EmissionLines[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedEmissionLinesSpectralDefinition)

    /** @group Optics */
    val surfaceEmissionLinesSpectralDefinition
      : Optional[Opportunity, SpectralDefinition.EmissionLines[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceEmissionLinesSpectralDefinition)

    /** @group Optics */
    val unnormalizedSED: Optional[Opportunity, Option[UnnormalizedSED]] =
      sourceProfile.andThen(SourceProfile.unnormalizedSED)

    /** @group Optics */
    val integratedBrightnesses
      : Optional[Opportunity, SortedMap[Band, BrightnessMeasure[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnesses)

    /** @group Optics */
    val surfaceBrightnesses: Optional[Opportunity, SortedMap[Band, BrightnessMeasure[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnesses)

    /** @group Optics */
    val integratedBrightnessesT: Traversal[Opportunity, BrightnessMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnessesT)

    /** @group Optics */
    val surfaceBrightnessesT: Traversal[Opportunity, BrightnessMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnessesT)

    /** @group Optics */
    def integratedBrightnessIn[T](
      b: Band
    ): Traversal[Opportunity, BrightnessMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedBrightnessIn(b))

    /** @group Optics */
    def surfaceBrightnessIn[T](b: Band): Traversal[Opportunity, BrightnessMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceBrightnessIn(b))

    /** @group Optics */
    val integratedWavelengthLines
      : Optional[Opportunity, SortedMap[Wavelength, EmissionLine[Integrated]]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLines)

    /** @group Optics */
    val surfaceWavelengthLines
      : Optional[Opportunity, SortedMap[Wavelength, EmissionLine[Surface]]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLines)

    /** @group Optics */
    val integratedWavelengthLinesT: Traversal[Opportunity, EmissionLine[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLinesT)

    /** @group Optics */
    val surfaceWavelengthLinesT: Traversal[Opportunity, EmissionLine[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLinesT)

    /** @group Optics */
    def integratedWavelengthLineIn(
      w: Wavelength
    ): Traversal[Opportunity, EmissionLine[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedWavelengthLineIn(w))

    /** @group Optics */
    def surfaceWavelengthLineIn[T](w: Wavelength): Traversal[Opportunity, EmissionLine[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceWavelengthLineIn(w))

    /** @group Optics */
    val integratedFluxDensityContinuum: Optional[Opportunity, FluxDensityContinuumMeasure[Integrated]] =
      sourceProfile.andThen(SourceProfile.integratedFluxDensityContinuum)

    /** @group Optics */
    val surfaceFluxDensityContinuum: Optional[Opportunity, FluxDensityContinuumMeasure[Surface]] =
      sourceProfile.andThen(SourceProfile.surfaceFluxDensityContinuum)
  }

}

trait TargetOptics { this: Target.type =>

  /** @group Optics */
  val sidereal: Prism[Target, Target.Sidereal] = GenPrism[Target, Target.Sidereal]

  /** @group Optics */
  val nonsidereal: Prism[Target, Target.Nonsidereal] = GenPrism[Target, Target.Nonsidereal]

    /** @group Optics */
  val opportunity: Prism[Target, Target.Opportunity] = GenPrism[Target, Target.Opportunity]

  /** @group Optics */
  val name: Lens[Target, NonEmptyString] =
    Lens[Target, NonEmptyString](_.name)(v => {
      case t @ Target.Sidereal(_, _, _, _) => Target.Sidereal.name.replace(v)(t)
      case t @ Target.Nonsidereal(_, _, _) => Target.Nonsidereal.name.replace(v)(t)
      case t @ Target.Opportunity(_, _, _) => Target.Opportunity.name.replace(v)(t)
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
      case t @ Target.Sidereal(_, _, _, _) => Target.Sidereal.sourceProfile.replace(v)(t)
      case t @ Target.Nonsidereal(_, _, _) => Target.Nonsidereal.sourceProfile.replace(v)(t)
      case t @ Target.Opportunity(_, _, _) => Target.Opportunity.sourceProfile.replace(v)(t)
    })

  val region: Optional[Target, Region] =
    opportunity.andThen(Opportunity.region)

  /** @group Optics */
  val integratedSpectralDefinition: Optional[Target, SpectralDefinition[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedSpectralDefinition)

  /** @group Optics */
  val surfaceSpectralDefinition: Optional[Target, SpectralDefinition[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceSpectralDefinition)

  /** @group Optics */
  val fwhm: Optional[Target, Angle] =
    sourceProfile.andThen(SourceProfile.fwhm)

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
  val unnormalizedSED: Optional[Target, Option[UnnormalizedSED]] =
    sourceProfile.andThen(SourceProfile.unnormalizedSED)

  /** @group Optics */
  val integratedBrightnesses: Optional[Target, SortedMap[Band, BrightnessMeasure[Integrated]]] =
    sourceProfile.andThen(SourceProfile.integratedBrightnesses)

  /** @group Optics */
  val surfaceBrightnesses: Optional[Target, SortedMap[Band, BrightnessMeasure[Surface]]] =
    sourceProfile.andThen(SourceProfile.surfaceBrightnesses)

  /** @group Optics */
  val integratedBrightnessesT: Traversal[Target, BrightnessMeasure[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedBrightnessesT)

  /** @group Optics */
  val surfaceBrightnessesT: Traversal[Target, BrightnessMeasure[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceBrightnessesT)

  /** @group Optics */
  def integratedBrightnessIn[T](
    b: Band
  ): Traversal[Target, BrightnessMeasure[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedBrightnessIn(b))

  /** @group Optics */
  def surfaceBrightnessIn[T](b: Band): Traversal[Target, BrightnessMeasure[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceBrightnessIn(b))

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
  val integratedFluxDensityContinuum: Optional[Target, FluxDensityContinuumMeasure[Integrated]] =
    sourceProfile.andThen(SourceProfile.integratedFluxDensityContinuum)

  /** @group Optics */
  val surfaceFluxDensityContinuum: Optional[Target, FluxDensityContinuumMeasure[Surface]] =
    sourceProfile.andThen(SourceProfile.surfaceFluxDensityContinuum)

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
    siderealTracking.andThen(SiderealTracking.baseRa)

  /** @group Optics */
  val baseDec: Optional[Target, Declination] =
    siderealTracking.andThen(SiderealTracking.baseDec)

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
}
