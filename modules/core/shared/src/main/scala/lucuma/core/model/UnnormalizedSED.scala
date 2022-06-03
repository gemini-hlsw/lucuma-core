// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order
import cats.data.NonEmptyMap
import cats.implicits._
import coulomb.*
import coulomb.cats.quantity.given
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.`enum`._
import lucuma.core.math.Wavelength
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

/**
 * Unnormalized Spectral Energy Distribution.
 *
 * An unnormalized SED describes the shape of the SED across wavelengths. It's then normalized
 * around a certain wavelength by using band brightnesses. By contrast, emission lines specify a
 * normalized SED directly, without the need for band brightness information.
 */
sealed trait UnnormalizedSED extends Product with Serializable

object UnnormalizedSED {

  final case class StellarLibrary(librarySpectrum: StellarLibrarySpectrum) extends UnnormalizedSED

  object StellarLibrary {
    implicit val eqStellarLibrary: Eq[StellarLibrary] = Eq.by(_.librarySpectrum)

    /** @group Optics */
    val librarySpectrum: Lens[StellarLibrary, StellarLibrarySpectrum] =
      Focus[StellarLibrary](_.librarySpectrum)
  }

  final case class CoolStarModel(temperature: CoolStarTemperature)
      extends UnnormalizedSED

  object CoolStarModel {
    implicit val orderCoolStarModel: Order[CoolStarModel] = Order.by(_.temperature)

    /** @group Optics */
    val temperature: Lens[CoolStarModel, CoolStarTemperature] =
      Focus[CoolStarModel](_.temperature)
  }

  final case class Galaxy(galaxySpectrum: GalaxySpectrum) extends UnnormalizedSED

  object Galaxy {
    implicit val eqGalaxy: Eq[Galaxy] = Eq.by(_.galaxySpectrum)

    /** @group Optics */
    val galaxySpectrum: Lens[Galaxy, GalaxySpectrum] =
      Focus[Galaxy](_.galaxySpectrum)
  }

  final case class Planet(planetSpectrum: PlanetSpectrum) extends UnnormalizedSED

  object Planet {
    implicit val eqPlanet: Eq[Planet] = Eq.by(_.planetSpectrum)

    /** @group Optics */
    val planetSpectrum: Lens[Planet, PlanetSpectrum] =
      Focus[Planet](_.planetSpectrum)
  }

  final case class Quasar(quasarSpectrum: QuasarSpectrum) extends UnnormalizedSED

  object Quasar {
    implicit val eqQuasar: Eq[Quasar] = Eq.by(_.quasarSpectrum)

    /** @group Optics */
    val quasarSpectrum: Lens[Quasar, QuasarSpectrum] =
      Focus[Quasar](_.quasarSpectrum)
  }

  final case class HIIRegion(hiiRegionSpectrum: HIIRegionSpectrum) extends UnnormalizedSED

  object HIIRegion {
    implicit val eqHIIRegion: Eq[HIIRegion] = Eq.by(_.hiiRegionSpectrum)

    /** @group Optics */
    val hiiRegionSpectrum: Lens[HIIRegion, HIIRegionSpectrum] =
      Focus[HIIRegion](_.hiiRegionSpectrum)
  }

  final case class PlanetaryNebula(planetaryNebulaSpectrum: PlanetaryNebulaSpectrum)
      extends UnnormalizedSED

  object PlanetaryNebula {
    implicit val eqPlanetaryNebula: Eq[PlanetaryNebula] = Eq.by(_.planetaryNebulaSpectrum)

    /** @group Optics */
    val planetaryNebulaSpectrum: Lens[PlanetaryNebula, PlanetaryNebulaSpectrum] =
      Focus[PlanetaryNebula](_.planetaryNebulaSpectrum)
  }

  /** Defined by power law function. */
  final case class PowerLaw(index: BigDecimal) extends UnnormalizedSED

  object PowerLaw {
    implicit val orderPowerLaw: Order[PowerLaw] = Order.by(_.index)

    /** @group Optics */
    val index: Lens[PowerLaw, BigDecimal] = Focus[PowerLaw](_.index)
  }

  /** A black body with a temperature in Kelvin. */
  final case class BlackBody(temperature: Quantity[PosInt, Kelvin]) extends UnnormalizedSED

  object BlackBody {
    implicit val orderBlackBody: Order[BlackBody] = Order.by(_.temperature)

    /** @group Optics */
    val temperature: Lens[BlackBody, Quantity[PosInt, Kelvin]] =
      Focus[BlackBody](_.temperature)
  }

  // Flux density is unitless since we just need the shape of the function. It can be in any applicable units.
  final case class UserDefined(fluxDensities: NonEmptyMap[Wavelength, PosBigDecimal])
      extends UnnormalizedSED

  object UserDefined {
    implicit val eqUserDefined: Eq[UserDefined] = Eq.by(_.fluxDensities)

    /** @group Optics */
    val fluxDensities: Lens[UserDefined, NonEmptyMap[Wavelength, PosBigDecimal]] =
      Focus[UserDefined](_.fluxDensities)
  }

  implicit val eqSpectralDistribution: Eq[UnnormalizedSED] =
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
  val stellarLibrary: Prism[UnnormalizedSED, StellarLibrary] =
    GenPrism[UnnormalizedSED, StellarLibrary]

  /** @group Optics */
  val coolStarModel: Prism[UnnormalizedSED, CoolStarModel] =
    GenPrism[UnnormalizedSED, CoolStarModel]

  /** @group Optics */
  val galaxy: Prism[UnnormalizedSED, Galaxy] =
    GenPrism[UnnormalizedSED, Galaxy]

  /** @group Optics */
  val planet: Prism[UnnormalizedSED, Planet] =
    GenPrism[UnnormalizedSED, Planet]

  /** @group Optics */
  val quasar: Prism[UnnormalizedSED, Quasar] =
    GenPrism[UnnormalizedSED, Quasar]

  /** @group Optics */
  val hiiRegion: Prism[UnnormalizedSED, HIIRegion] =
    GenPrism[UnnormalizedSED, HIIRegion]

  /** @group Optics */
  val planetaryNebula: Prism[UnnormalizedSED, PlanetaryNebula] =
    GenPrism[UnnormalizedSED, PlanetaryNebula]

  /** @group Optics */
  val powerLaw: Prism[UnnormalizedSED, PowerLaw] =
    GenPrism[UnnormalizedSED, PowerLaw]

  /** @group Optics */
  val blackBody: Prism[UnnormalizedSED, BlackBody] =
    GenPrism[UnnormalizedSED, BlackBody]

  /** @group Optics */
  val userDefined: Prism[UnnormalizedSED, UserDefined] =
    GenPrism[UnnormalizedSED, UserDefined]
}
