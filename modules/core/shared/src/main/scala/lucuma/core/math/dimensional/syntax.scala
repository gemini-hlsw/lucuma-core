// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import coulomb.Quantity
import lucuma.core.util.*

object syntax:
  extension[N, U](quantity: Quantity[N, U])

    /**
    * Convert a coulomb `Quantity` to a `Measure` with runtime unit representation.
    */
    def toMeasure(using unit: UnitOfMeasure[U]): Measure[N] = Measure(quantity.value, unit)

    /**
    * Convert a coulomb `Quantity` to a `Measure` with runtime unit representation and tag `Tag`.
    */
    def toMeasureTagged[T](using ev: TaggedUnit[U, T]): Measure[N] Of T = {
      val tagged: Measure[N] Of T = tag[T](Measure(quantity.value, ev.unit))
      tagged
    }
