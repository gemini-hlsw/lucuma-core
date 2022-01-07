// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb.Quantity
import coulomb.unitops.UnitString
import lucuma.core.util.TypeString
import shapeless.tag
import shapeless.tag.@@

package object dimensional {
  type Of[+T, U] = @@[T, U]

  implicit class QuantityOps[N, U](quantity: Quantity[N, U]) {

    /**
     * Convert a coulomb `Quantity` to a `Measure` with runtime unit representation.
     */
    def toMeasure(implicit unit: UnitOfMeasure[U]): Measure[N] = Measure(quantity.value, unit)

    /**
     * Convert a coulomb `Quantity` to a `Measure` with runtime unit representation and tag `Tag`.
     */
    def toMeasureTagged[T](implicit ev: TaggedUnit[U, T]): Measure[N] Of T = {
      val tagged: Measure[N] @@ T = tag[T](Measure(quantity.value, ev.unit))
      tagged
    }
  }

  // Default `TypeString` for a unit type.
  implicit def typeStringFromUnitString[U](implicit ev: UnitString[U]): TypeString[U] =
    new TypeString[U] {
      val serialized: String = ev.abbv
    }
}
