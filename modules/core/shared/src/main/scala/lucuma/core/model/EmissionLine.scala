// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.implicits.*
import coulomb.*
import coulomb.ops.algebra.cats.all.given
import eu.timepit.refined.cats.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.dimensional.*
import monocle.Focus
import monocle.Lens

final case class EmissionLine[T](lineWidth: LineWidthQuantity, lineFlux: LineFluxMeasure[T]) {
  /**
   * Convert units to `T0` brightness type.
   *
   * @tparam `T0`
   *   `Integrated` or `Surface`
   */
  def to[T0](using conv: TagConverter[LineFlux[T], LineFlux[T0]]): EmissionLine[T0] =
    EmissionLine[T0](lineWidth, lineFlux.toTag[LineFlux[T0]])
}

object EmissionLine {
  given eqEmissionLine[T]: Eq[EmissionLine[T]] =
    Eq.by(x => (x.lineWidth, x.lineFlux))

  /** @group Optics */
  def lineWidth[T]: Lens[EmissionLine[T], LineWidthQuantity] =
    Focus[EmissionLine[T]](_.lineWidth)

  /** @group Optics */
  def lineFlux[T]: Lens[EmissionLine[T], LineFluxMeasure[T]] =
    Focus[EmissionLine[T]](_.lineFlux)
}
