// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb.Quantity
import lucuma.core.util.TypeString
import coulomb.ops.ShowUnit

package object dimensional {
  opaque infix type Of[+T, U] <: T = T
  inline def tag[U]: Tagger[U] = Tagger()
  final class Tagger[U] {
    inline def apply[T](t: T): T Of U = t
  }

  implicit class QuantityOps[N, U](quantity: Quantity[N, U]) {

    /**
     * Convert a coulomb `Quantity` to a `Measure` with runtime unit representation.
     */
    def toMeasure(implicit unit: UnitOfMeasure[U]): Measure[N] = Measure(quantity.value, unit)

    /**
     * Convert a coulomb `Quantity` to a `Measure` with runtime unit representation and tag `Tag`.
     */
    def toMeasureTagged[T](implicit ev: TaggedUnit[U, T]): Measure[N] Of T = {
      val tagged: Measure[N] Of T = tag[T](Measure(quantity.value, ev.unit))
      tagged
    }
  }

  // Default `TypeString` for a unit type.
  implicit def typeStringFromUnitString[U](implicit ev: ShowUnit[U]): TypeString[U] =
    new TypeString[U] {
      val serialized: String = ev.value
    }
}
