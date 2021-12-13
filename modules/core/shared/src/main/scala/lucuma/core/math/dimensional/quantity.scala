// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import coulomb._
import monocle.Focus
import monocle.Lens
import _root_.cats.kernel.Eq

/**
 * A magnitude of type `N` and a runtime representation of a physical unit.
 */
trait Qty[N] {
  val value: N
  val unit: UnitType

  /**
   * Convert to `coulomb.Quantity`.
   */
  def toCoulomb: Quantity[N, unit.Type] = value.withUnit[unit.Type]
}

object Qty {
  implicit def eqQty[N: Eq]: Eq[Qty[N]] = Eq.by(x => (x.value, x.unit))
}

/**
 * A magnitude of type `N` and a runtime representation of a physical unit in group `UG`.
 */
case class GroupedUnitQuantity[N, +UG] private (value: N, unit: GroupedUnitType[UG]) extends Qty[N]
object GroupedUnitQuantity {

  /**
   * Create a `GroupedUnitQuantity` from a `coulomb.Quantity`.
   */
  def apply[N, UG, U](
    q:             Quantity[N, U]
  )(implicit unit: GroupedUnitOfMeasure[UG, U]): GroupedUnitQuantity[N, UG] =
    GroupedUnitQuantity(q.value, unit)

  def value[N, UG]: Lens[GroupedUnitQuantity[N, UG], N] = Focus[GroupedUnitQuantity[N, UG]](_.value)

  def unit[N, UG]: Lens[GroupedUnitQuantity[N, UG], GroupedUnitType[UG]] =
    Focus[GroupedUnitQuantity[N, UG]](_.unit)

  implicit def eqGroupedUnitQuantity[N, UG]: Eq[GroupedUnitQuantity[N, UG]] = Eq.fromUniversalEquals
}
