// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order
import cats.implicits._
import coulomb.Quantity
import coulomb.cats.implicits._
import coulomb.si.Kelvin
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum.NonStellarLibrarySpectrum
import lucuma.core.enum.StellarLibrarySpectrum
// import lucuma.core.math.dimensional.Qty
import lucuma.core.math.dimensional.GroupedUnitQuantity
import lucuma.core.math.BrightnessUnit._

// B = Brightness unit group; used only in EmissionLine for the moment
sealed trait SpectralDistribution[+T] extends Serializable

// TODO Lenses

object SpectralDistribution {

  /** A library defined spectrum. */
  final case class Library(
    // TODO The mockup at https://media.app.shortcut.com/api/attachments/files/clubhouse-assets/5f493112-dac1-417b-83f7-b20a9b5a40b6/61a51fbc-67bb-423c-b25b-f34640956c52/Explore_Targets_Source_Definition.png
    // It doesn't mention non-stellar library. Is it another way of modeling Galaxy, Planet, Quasar, HII Region and Planetary Nebula?
    librarySpectrum: Either[StellarLibrarySpectrum, NonStellarLibrarySpectrum]
  ) extends SpectralDistribution[Nothing]

  object Library {
    implicit val eqLibrary: Eq[Library] = Eq.by(_.librarySpectrum)
  }

  final case class CoolStarModel(temperature: Quantity[PosBigDecimal, Kelvin])
      extends SpectralDistribution[Nothing]

  object CoolStarModel {
    implicit val orderCoolStarModel: Order[CoolStarModel] = Order.by(_.temperature)
  }

  // Galaxy

  // Planet

  // Quasar

  // HII Region

  // Planetary Nebula

  // EmissionLine
  // TODO Check if BigDecimal [parse from/toString to] "5e-19"

  // Both line and continuum have to be specified. It's OK for both units not be congruent.
  final case class EmissionLine[+T](
    line:      GroupedUnitQuantity[BigDecimal, LineFlux[T]],
    continuum: GroupedUnitQuantity[BigDecimal, ContinuumFluxDensity[T]]
  ) extends SpectralDistribution[T]
  object EmissionLine {
    implicit def eqEmissionLine[T]: Eq[EmissionLine[T]] =
      Eq.by(x => (x.line, x.continuum))
  }

  /** Defined by power law function. */
  final case class PowerLaw(index: BigDecimal) extends SpectralDistribution[Nothing]

  object PowerLaw {
    implicit val orderPowerLaw: Order[PowerLaw] = Order.by(_.index)
  }

  /** A black body with a temperature in Kelvin. */
  final case class BlackBody(temperature: Quantity[PosBigDecimal, Kelvin])
      extends SpectralDistribution[Nothing]

  object BlackBody {
    implicit val orderBlackBody: Order[BlackBody] = Order.by(_.temperature)
  }

  // TODO UserDefined

  implicit def eqSpectralDistribution[B]: Eq[SpectralDistribution[B]] =
    Eq.instance {
      case (a @ Library(_), b @ Library(_))                 => a === b
      case (a @ CoolStarModel(_), b @ CoolStarModel(_))     => a === b
      case (a @ BlackBody(_), b @ BlackBody(_))             => a === b
      case (a @ EmissionLine(_, _), b @ EmissionLine(_, _)) => a === b
      case (a @ PowerLaw(_), b @ PowerLaw(_))               => a === b
      case _                                                => false
    }
}
