// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import coulomb.define._

// All of this is a bridge between coulomb an runtime quantities (as defined in `Qty`).

/**
 * Runtime representation of a physical unit. Wraps:
 *   - A type (usually phantom), meant to be `coulomb`'s type for the unit; and
 *   - A `UnitDefinition`, which is a common trait of `coulomb`'s `BaseUnit` and `DerivedUnit`.
 *
 * In `coulomb` units are types. However in some cases we want to have runtime representations of
 * units.
 */
trait UnitType { self =>
  type Type
  def definition: UnitDefinition

  def withValue[N](_value: N): Qty[N] = new Qty[N] {
    val value = _value
    val unit  = self
  }
}

/**
 * Type-parametrized runtime representation of physical unit `U`.
 *
 * Can be automatically derived if there's an implicit `BaseUnit[U]` or `DerivedUnit[U, _]` in
 * scope.
 */
trait UnitOfMeasure[U] extends UnitType {
  type Type = U

  /**
   * Build an association between this unit and group `G`.
   */
  def groupedIn[G]: GroupedUnitOfMeasure[G, U] = GroupedUnitOfMeasure(this)
}

object UnitOfMeasure {
  def apply[U: UnitOfMeasure]: UnitOfMeasure[U] = implicitly[UnitOfMeasure[U]]

  implicit def unitOfMeasureFromBaseUnit[U](implicit ev: BaseUnit[U]): UnitOfMeasure[U] =
    new UnitOfMeasure[U] {
      val definition = ev
    }

  implicit def unitOfMeasureFromDerivedUnit[U, D](implicit
    ev: DerivedUnit[U, D]
  ): UnitOfMeasure[U] =
    new UnitOfMeasure[U] {
      val definition = ev
    }
}

/**
 * Runtime association of a `UnitType` to group `G`.
 *
 * The group doesn't exist in runtime, it's just a type tag. It's covariant since we can have a
 * hierarchy of groups. Eg: We have unit groups for "integral brightness" and "surface brightness",
 * and they are both subgroups of the larger "brightness" group.
 */
trait GroupedUnitType[+G] extends UnitType {
  override def withValue[N](value: N): GroupedUnitQuantity[N, G] =
    GroupedUnitQuantity(value, this)
}

/**
 * Type-parametrized runtime representation of physical unit `U` and its association to unit group
 * `G`.
 */
trait GroupedUnitOfMeasure[+G, U] extends UnitOfMeasure[U] with GroupedUnitType[G]

object GroupedUnitOfMeasure {
  def apply[G, U](implicit unit: UnitOfMeasure[U]): GroupedUnitOfMeasure[G, U] =
    new GroupedUnitOfMeasure[G, U] {
      val definition = unit.definition
    }
}
