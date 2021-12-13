// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import coulomb.define._
import cats.Eq

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

object UnitType {
  // `UnitDefiniton`s are expected to be singletons.
  implicit val eqUnitType: Eq[UnitType] = Eq.instance((a, b) => a.definition == b.definition)
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
 * Runtime association of a `UnitType` to unit group `UG`.
 *
 * The group doesn't exist in runtime, it's just a type tag.
 */
trait GroupedUnitType[+UG] extends UnitType {
  override def withValue[N](value: N): GroupedUnitQuantity[N, UG] =
    GroupedUnitQuantity(value, this)
}

/**
 * Type-parametrized runtime representation of physical unit `U` and its association to unit group
 * `UG`.
 */
trait GroupedUnitOfMeasure[UG, U] extends UnitOfMeasure[U] with GroupedUnitType[UG]

object GroupedUnitOfMeasure {
  def apply[UG, U](implicit unit: UnitOfMeasure[U]): GroupedUnitOfMeasure[UG, U] =
    new GroupedUnitOfMeasure[UG, U] {
      val definition = unit.definition
    }
}