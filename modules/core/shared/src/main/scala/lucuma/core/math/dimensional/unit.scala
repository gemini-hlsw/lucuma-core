// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import coulomb.unitops.UnitString
import lucuma.core.util.Display

import java.util.Objects

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

  /** the full name of a unit, e.g. "meter" */
  def name: String

  /** the abbreviation of a unit, e.g. "m" for "meter" */
  def abbv: String

  def withValue[N](_value: N): Qty[N] = new Qty[N] {
    val value = _value
    val unit  = self
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: UnitType => name == that.name && abbv == that.abbv
    case _              => false
  }

  override def hashCode: Int = Objects.hash(name, abbv)

  override def toString: String = abbv
}

object UnitType {
  implicit val eqUnitType: Eq[UnitType] = Eq.fromUniversalEquals

  implicit def displayUnitType: Display[UnitType] = Display.by(_.abbv, _.name)
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

  implicit def unitOfMeasureFromUnitString[U](implicit ev: UnitString[U]): UnitOfMeasure[U] =
    new UnitOfMeasure[U] {
      override type Type = U
      override val name = ev.full
      override val abbv = ev.abbv
    }

  implicit def eqUnitOfMeasure[U]: Eq[UnitOfMeasure[U]] = Eq.fromUniversalEquals
}

/**
 * Runtime association of a `UnitType` to unit group `UG`.
 *
 * The group doesn't exist in runtime, it's just a type tag.
 */
trait GroupedUnitType[+UG] extends UnitType {
  override def withValue[N](value: N): GroupedUnitQty[N, UG] =
    GroupedUnitQty(value, this)

  def ungrouped: UnitType = this
}

object GroupedUnitType {
  implicit def eqGroupedUnitType[UG]: Eq[GroupedUnitType[UG]] = Eq.fromUniversalEquals
}

/**
 * Type-parametrized runtime representation of physical unit `U` and its association to unit group
 * `UG`.
 */
trait GroupedUnitOfMeasure[UG, U] extends UnitOfMeasure[U] with GroupedUnitType[UG] {
  override def ungrouped: UnitOfMeasure[U] = this
}

object GroupedUnitOfMeasure {
  def apply[UG, U](implicit unit: UnitOfMeasure[U]): GroupedUnitOfMeasure[UG, U] =
    new GroupedUnitOfMeasure[UG, U] {
      val name = unit.name
      val abbv = unit.abbv
    }

  // `UnitDefiniton`s are expected to be singletons.
  implicit def eqGroupedUnitOfMeasure[UG, U]: Eq[GroupedUnitOfMeasure[UG, U]] =
    Eq.fromUniversalEquals
}
