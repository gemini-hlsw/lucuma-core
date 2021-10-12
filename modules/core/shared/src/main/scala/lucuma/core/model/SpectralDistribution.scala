// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import coulomb.Quantity
import coulomb.si.Kelvin
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum.NonStellarLibrarySpectrum
import lucuma.core.enum.StellarLibrarySpectrum

sealed trait SpectralDistribution extends Serializable

object SpectralDistribution {

  /** A black body with a temperature in Kelvin. */
  final case class BlackBody(temperature: Quantity[PosBigDecimal, Kelvin])
      extends SpectralDistribution

  /** Defined by power law function. */
  final case class PowerLaw(index: BigDecimal) extends SpectralDistribution

  /** A library defined spectrum. */
  final case class Library(
    librarySpectrum: Either[StellarLibrarySpectrum, NonStellarLibrarySpectrum]
  ) extends SpectralDistribution

  // TODO: emission line and user-defined

}
