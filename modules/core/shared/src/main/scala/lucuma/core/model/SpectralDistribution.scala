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
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.GroupedUnitQty
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.Traversal
import monocle.macros.GenPrism

import scala.collection.immutable.SortedMap

// T = Brightness type (integral or surface); used only in EmissionLine
sealed trait SpectralDistribution[+T] extends Serializable

object SpectralDistribution {

  final case class StellarLibrary(librarySpectrum: StellarLibrarySpectrum)
      extends SpectralDistribution[Nothing]

  object StellarLibrary {
    implicit val eqStellarLibrary: Eq[StellarLibrary] = Eq.by(_.librarySpectrum)

    /** @group Optics */
    val librarySpectrum: Lens[StellarLibrary, StellarLibrarySpectrum] =
      Focus[StellarLibrary](_.librarySpectrum)
  }

  final case class CoolStarModel(temperature: Quantity[PosBigDecimal, Kelvin])
      extends SpectralDistribution[Nothing]

  object CoolStarModel {
    implicit val orderCoolStarModel: Order[CoolStarModel] = Order.by(_.temperature)

    /** @group Optics */
    val temperature: Lens[CoolStarModel, Quantity[PosBigDecimal, Kelvin]] =
      Focus[CoolStarModel](_.temperature)
  }

  final case class Galaxy(galaxySpectrum: GalaxySpectrum) extends SpectralDistribution[Nothing]

  object Galaxy {
    implicit val eqGalaxy: Eq[Galaxy] = Eq.by(_.galaxySpectrum)

    /** @group Optics */
    val galaxySpectrum: Lens[Galaxy, GalaxySpectrum] =
      Focus[Galaxy](_.galaxySpectrum)
  }

  final case class Planet(planetSpectrum: PlanetSpectrum) extends SpectralDistribution[Nothing]

  object Planet {
    implicit val eqPlanet: Eq[Planet] = Eq.by(_.planetSpectrum)

    /** @group Optics */
    val planetSpectrum: Lens[Planet, PlanetSpectrum] =
      Focus[Planet](_.planetSpectrum)
  }

  final case class Quasar(quasarSpectrum: QuasarSpectrum) extends SpectralDistribution[Nothing]

  object Quasar {
    implicit val eqQuasar: Eq[Quasar] = Eq.by(_.quasarSpectrum)

    /** @group Optics */
    val quasarSpectrum: Lens[Quasar, QuasarSpectrum] =
      Focus[Quasar](_.quasarSpectrum)
  }

  final case class HIIRegion(hiiRegionSpectrum: HIIRegionSpectrum)
      extends SpectralDistribution[Nothing]

  object HIIRegion {
    implicit val eqHIIRegion: Eq[HIIRegion] = Eq.by(_.hiiRegionSpectrum)

    /** @group Optics */
    val hiiRegionSpectrum: Lens[HIIRegion, HIIRegionSpectrum] =
      Focus[HIIRegion](_.hiiRegionSpectrum)
  }

  final case class PlanetaryNebula(planetaryNebulaSpectrum: PlanetaryNebulaSpectrum)
      extends SpectralDistribution[Nothing]

  object PlanetaryNebula {
    implicit val eqPlanetaryNebula: Eq[PlanetaryNebula] = Eq.by(_.planetaryNebulaSpectrum)

    /** @group Optics */
    val planetaryNebulaSpectrum: Lens[PlanetaryNebula, PlanetaryNebulaSpectrum] =
      Focus[PlanetaryNebula](_.planetaryNebulaSpectrum)
  }

  // TODO Check if BigDecimal [parse from/toString to] "5e-19".
  final case class EmissionLines[+T](
    lines:                SortedMap[Wavelength, EmissionLine[T]],
    fluxDensityContinuum: GroupedUnitQty[PosBigDecimal, FluxDensityContinuum[T]]
  ) extends SpectralDistribution[T]

  object EmissionLines {
    implicit def eqEmissionLines[T]: Eq[EmissionLines[T]] =
      Eq.by(x => (x.lines, x.fluxDensityContinuum))

    /** @group Optics */
    def lines[T]: Lens[EmissionLines[T], SortedMap[Wavelength, EmissionLine[T]]] =
      Focus[EmissionLines[T]](_.lines)

    /** @group Optics */
    def linesT[T]: Traversal[EmissionLines[T], EmissionLine[T]] =
      lines.each

    /** @group Optics */
    def fluxDensityContinuum[T]
      : Lens[EmissionLines[T], GroupedUnitQty[PosBigDecimal, FluxDensityContinuum[T]]] =
      Focus[EmissionLines[T]](_.fluxDensityContinuum)
  }

  /** Defined by power law function. */
  final case class PowerLaw(index: BigDecimal) extends SpectralDistribution[Nothing]

  object PowerLaw {
    implicit val orderPowerLaw: Order[PowerLaw] = Order.by(_.index)

    /** @group Optics */
    val index: Lens[PowerLaw, BigDecimal] = Focus[PowerLaw](_.index)
  }

  /** A black body with a temperature in Kelvin. */
  final case class BlackBody(temperature: Quantity[PosBigDecimal, Kelvin])
      extends SpectralDistribution[Nothing]

  object BlackBody {
    implicit val orderBlackBody: Order[BlackBody] = Order.by(_.temperature)

    /** @group Optics */
    val temperature: Lens[BlackBody, Quantity[PosBigDecimal, Kelvin]] =
      Focus[BlackBody](_.temperature)
  }

  // Flux density is unitless since we just need the shape of the function. It can be in any applicable units.
  final case class UserDefined(fluxDensities: NonEmptyMap[Wavelength, PosBigDecimal])
      extends SpectralDistribution[Nothing]

  object UserDefined {
    implicit val eqUserDefined: Eq[UserDefined] = Eq.by(_.fluxDensities)

    /** @group Optics */
    val fluxDensities: Lens[UserDefined, NonEmptyMap[Wavelength, PosBigDecimal]] =
      Focus[UserDefined](_.fluxDensities)
  }

  implicit def eqSpectralDistribution[B]: Eq[SpectralDistribution[B]] =
    Eq.instance {
      case (a @ StellarLibrary(_), b @ StellarLibrary(_))     => a === b
      case (a @ CoolStarModel(_), b @ CoolStarModel(_))       => a === b
      case (a @ Galaxy(_), b @ Galaxy(_))                     => a === b
      case (a @ Planet(_), b @ Planet(_))                     => a === b
      case (a @ Quasar(_), b @ Quasar(_))                     => a === b
      case (a @ HIIRegion(_), b @ HIIRegion(_))               => a === b
      case (a @ PlanetaryNebula(_), b @ PlanetaryNebula(_))   => a === b
      case (a @ EmissionLines(_, _), b @ EmissionLines(_, _)) => a === b
      case (a @ PowerLaw(_), b @ PowerLaw(_))                 => a === b
      case (a @ BlackBody(_), b @ BlackBody(_))               => a === b
      case (a @ UserDefined(_), b @ UserDefined(_))           => a === b
      case _                                                  => false
    }

  /** @group Optics */
  def stellarLibrary[T]: Prism[SpectralDistribution[T], StellarLibrary] =
    GenPrism[SpectralDistribution[T], StellarLibrary]

  /** @group Optics */
  def coolStarModel[T]: Prism[SpectralDistribution[T], CoolStarModel] =
    GenPrism[SpectralDistribution[T], CoolStarModel]

  /** @group Optics */
  def galaxy[T]: Prism[SpectralDistribution[T], Galaxy] =
    GenPrism[SpectralDistribution[T], Galaxy]

  /** @group Optics */
  def planet[T]: Prism[SpectralDistribution[T], Planet] =
    GenPrism[SpectralDistribution[T], Planet]

  /** @group Optics */
  def quasar[T]: Prism[SpectralDistribution[T], Quasar] =
    GenPrism[SpectralDistribution[T], Quasar]

  /** @group Optics */
  def hiiRegion[T]: Prism[SpectralDistribution[T], HIIRegion] =
    GenPrism[SpectralDistribution[T], HIIRegion]

  /** @group Optics */
  def planetaryNebula[T]: Prism[SpectralDistribution[T], PlanetaryNebula] =
    GenPrism[SpectralDistribution[T], PlanetaryNebula]

  /** @group Optics */
  def emissionLines[T]: Prism[SpectralDistribution[T], EmissionLines[T]] =
    GenPrism[SpectralDistribution[T], EmissionLines[T]]

  /** @group Optics */
  def powerLaw[T]: Prism[SpectralDistribution[T], PowerLaw] =
    GenPrism[SpectralDistribution[T], PowerLaw]

  /** @group Optics */
  def blackBody[T]: Prism[SpectralDistribution[T], BlackBody] =
    GenPrism[SpectralDistribution[T], BlackBody]

  /** @group Optics */
  def userDefined[T]: Prism[SpectralDistribution[T], UserDefined] =
    GenPrism[SpectralDistribution[T], UserDefined]
}
