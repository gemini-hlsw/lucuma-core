// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.implicits._
import coulomb._
import coulomb.cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import monocle.Focus
import monocle.Lens

final case class EmissionLine[T](
  wavelength: Wavelength,
  lineWidth:  Quantity[PosBigDecimal, KilometersPerSecond],
  lineFlux:   Measure[PosBigDecimal] Of LineFlux[T]
) {

  /**
   * Convert units to `T0` brightness type.
   *
   * @tparam `T0`
   *   `Integrated` or `Surface`
   */
  def to[T0](implicit conv: TagConverter[LineFlux[T], LineFlux[T0]]): EmissionLine[T0] =
    EmissionLine[T0](wavelength, lineWidth, lineFlux.toTag[LineFlux[T0]])
}

object EmissionLine {
  implicit def eqEmissionLine[T]: Eq[EmissionLine[T]] =
    Eq.by(x => (x.wavelength, x.lineWidth, x.lineFlux))

  /** @group Optics */
  def wavelength[T]: Lens[EmissionLine[T], Wavelength] =
    Focus[EmissionLine[T]](_.wavelength)

  /** @group Optics */
  def lineWidth[T]: Lens[EmissionLine[T], Quantity[PosBigDecimal, KilometersPerSecond]] =
    Focus[EmissionLine[T]](_.lineWidth)

  /** @group Optics */
  def lineFlux[T]: Lens[EmissionLine[T], Measure[PosBigDecimal] Of LineFlux[T]] =
    Focus[EmissionLine[T]](_.lineFlux)
}
