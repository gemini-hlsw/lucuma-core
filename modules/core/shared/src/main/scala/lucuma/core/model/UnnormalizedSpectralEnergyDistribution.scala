// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order
import cats.data.NonEmptyMap
import cats.implicits._
import coulomb._
import coulomb.cats.implicits._
import coulomb.si.Kelvin
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum._
import lucuma.core.math.Wavelength
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait UnnormalizedSpectralEnergyDistribution extends Product with Serializable

object UnnormalizedSpectralEnergyDistribution {

  final case class StellarLibrary(librarySpectrum: StellarLibrarySpectrum)
      extends UnnormalizedSpectralEnergyDistribution

  object StellarLibrary {
    implicit val eqStellarLibrary: Eq[StellarLibrary] = Eq.by(_.librarySpectrum)

    /** @group Optics */
    val librarySpectrum: Lens[StellarLibrary, StellarLibrarySpectrum] =
      Focus[StellarLibrary](_.librarySpectrum)
  }

  final case class CoolStarModel(temperature: Quantity[PosBigDecimal, Kelvin])
      extends UnnormalizedSpectralEnergyDistribution

  object CoolStarModel {
    implicit val orderCoolStarModel: Order[CoolStarModel] = Order.by(_.temperature)

    /** @group Optics */
    val temperature: Lens[CoolStarModel, Quantity[PosBigDecimal, Kelvin]] =
      Focus[CoolStarModel](_.temperature)
  }

  final case class Galaxy(galaxySpectrum: GalaxySpectrum)
      extends UnnormalizedSpectralEnergyDistribution

  object Galaxy {
    implicit val eqGalaxy: Eq[Galaxy] = Eq.by(_.galaxySpectrum)

    /** @group Optics */
    val galaxySpectrum: Lens[Galaxy, GalaxySpectrum] =
      Focus[Galaxy](_.galaxySpectrum)
  }

  final case class Planet(planetSpectrum: PlanetSpectrum)
      extends UnnormalizedSpectralEnergyDistribution

  object Planet {
    implicit val eqPlanet: Eq[Planet] = Eq.by(_.planetSpectrum)

    /** @group Optics */
    val planetSpectrum: Lens[Planet, PlanetSpectrum] =
      Focus[Planet](_.planetSpectrum)
  }

  final case class Quasar(quasarSpectrum: QuasarSpectrum)
      extends UnnormalizedSpectralEnergyDistribution

  object Quasar {
    implicit val eqQuasar: Eq[Quasar] = Eq.by(_.quasarSpectrum)

    /** @group Optics */
    val quasarSpectrum: Lens[Quasar, QuasarSpectrum] =
      Focus[Quasar](_.quasarSpectrum)
  }

  final case class HIIRegion(hiiRegionSpectrum: HIIRegionSpectrum)
      extends UnnormalizedSpectralEnergyDistribution

  object HIIRegion {
    implicit val eqHIIRegion: Eq[HIIRegion] = Eq.by(_.hiiRegionSpectrum)

    /** @group Optics */
    val hiiRegionSpectrum: Lens[HIIRegion, HIIRegionSpectrum] =
      Focus[HIIRegion](_.hiiRegionSpectrum)
  }

  final case class PlanetaryNebula(planetaryNebulaSpectrum: PlanetaryNebulaSpectrum)
      extends UnnormalizedSpectralEnergyDistribution

  object PlanetaryNebula {
    implicit val eqPlanetaryNebula: Eq[PlanetaryNebula] = Eq.by(_.planetaryNebulaSpectrum)

    /** @group Optics */
    val planetaryNebulaSpectrum: Lens[PlanetaryNebula, PlanetaryNebulaSpectrum] =
      Focus[PlanetaryNebula](_.planetaryNebulaSpectrum)
  }

  /** Defined by power law function. */
  final case class PowerLaw(index: BigDecimal) extends UnnormalizedSpectralEnergyDistribution

  object PowerLaw {
    implicit val orderPowerLaw: Order[PowerLaw] = Order.by(_.index)

    /** @group Optics */
    val index: Lens[PowerLaw, BigDecimal] = Focus[PowerLaw](_.index)
  }

  /** A black body with a temperature in Kelvin. */
  final case class BlackBody(temperature: Quantity[PosBigDecimal, Kelvin])
      extends UnnormalizedSpectralEnergyDistribution

  object BlackBody {
    implicit val orderBlackBody: Order[BlackBody] = Order.by(_.temperature)

    /** @group Optics */
    val temperature: Lens[BlackBody, Quantity[PosBigDecimal, Kelvin]] =
      Focus[BlackBody](_.temperature)
  }

  // Flux density is unitless since we just need the shape of the function. It can be in any applicable units.
  final case class UserDefined(fluxDensities: NonEmptyMap[Wavelength, PosBigDecimal])
      extends UnnormalizedSpectralEnergyDistribution

  object UserDefined {
    implicit val eqUserDefined: Eq[UserDefined] = Eq.by(_.fluxDensities)

    /** @group Optics */
    val fluxDensities: Lens[UserDefined, NonEmptyMap[Wavelength, PosBigDecimal]] =
      Focus[UserDefined](_.fluxDensities)
  }

  implicit val eqSpectralDistribution: Eq[UnnormalizedSpectralEnergyDistribution] =
    Eq.instance {
      case (a @ StellarLibrary(_), b @ StellarLibrary(_))   => a === b
      case (a @ CoolStarModel(_), b @ CoolStarModel(_))     => a === b
      case (a @ Galaxy(_), b @ Galaxy(_))                   => a === b
      case (a @ Planet(_), b @ Planet(_))                   => a === b
      case (a @ Quasar(_), b @ Quasar(_))                   => a === b
      case (a @ HIIRegion(_), b @ HIIRegion(_))             => a === b
      case (a @ PlanetaryNebula(_), b @ PlanetaryNebula(_)) => a === b
      case (a @ PowerLaw(_), b @ PowerLaw(_))               => a === b
      case (a @ BlackBody(_), b @ BlackBody(_))             => a === b
      case (a @ UserDefined(_), b @ UserDefined(_))         => a === b
      case _                                                => false
    }

  /** @group Optics */
  val stellarLibrary: Prism[UnnormalizedSpectralEnergyDistribution, StellarLibrary] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, StellarLibrary]

  /** @group Optics */
  val coolStarModel: Prism[UnnormalizedSpectralEnergyDistribution, CoolStarModel] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, CoolStarModel]

  /** @group Optics */
  val galaxy: Prism[UnnormalizedSpectralEnergyDistribution, Galaxy] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, Galaxy]

  /** @group Optics */
  val planet: Prism[UnnormalizedSpectralEnergyDistribution, Planet] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, Planet]

  /** @group Optics */
  val quasar: Prism[UnnormalizedSpectralEnergyDistribution, Quasar] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, Quasar]

  /** @group Optics */
  val hiiRegion: Prism[UnnormalizedSpectralEnergyDistribution, HIIRegion] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, HIIRegion]

  /** @group Optics */
  val planetaryNebula: Prism[UnnormalizedSpectralEnergyDistribution, PlanetaryNebula] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, PlanetaryNebula]

  /** @group Optics */
  val powerLaw: Prism[UnnormalizedSpectralEnergyDistribution, PowerLaw] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, PowerLaw]

  /** @group Optics */
  val blackBody: Prism[UnnormalizedSpectralEnergyDistribution, BlackBody] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, BlackBody]

  /** @group Optics */
  val userDefined: Prism[UnnormalizedSpectralEnergyDistribution, UserDefined] =
    GenPrism[UnnormalizedSpectralEnergyDistribution, UserDefined]
}
