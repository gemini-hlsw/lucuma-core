// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyMap
import cats.implicits._
import coulomb.*
import coulomb.ops.algebra.cats.all.given
import coulomb.syntax.*
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Lens

final case class EmissionLine[T](
  lineWidth: Quantity[PosBigDecimal, KilometersPerSecond],
  lineFlux:  EmissionLine.LineFluxOverTime[T]
) {

  /**
   * Convert units to `T0` brightness type.
   *
   * @tparam `T0`
   *   `Integrated` or `Surface`
   */
  def to[T0](implicit conv: TagConverter[LineFlux[T], LineFlux[T0]]): EmissionLine[T0] =
    EmissionLine[T0](lineWidth, lineFlux.map(_.toTag[LineFlux[T0]]))
}

object EmissionLine {
  type LineFluxOverTime[T] = NonEmptyMap[Timestamp, Measure[PosBigDecimal] Of LineFlux[T]]
  implicit def eqEmissionLine[T]: Eq[EmissionLine[T]] =
    Eq.by(x => (x.lineWidth, x.lineFlux))

  /** @group Optics */
  def lineWidth[T]: Lens[EmissionLine[T], Quantity[PosBigDecimal, KilometersPerSecond]] =
    Focus[EmissionLine[T]](_.lineWidth)

  /** @group Optics */
  def lineFlux[T]: Lens[EmissionLine[T], NonEmptyMap[Timestamp, Measure[PosBigDecimal] Of LineFlux[T]]] =
    Focus[EmissionLine[T]](_.lineFlux)
}
