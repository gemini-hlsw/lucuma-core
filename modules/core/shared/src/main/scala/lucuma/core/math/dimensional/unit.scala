// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import coulomb.unitops.UnitString
import lucuma.core.util.Display
import lucuma.core.util.IsTagged
import shapeless.tag
import shapeless.tag.@@

import java.util.Objects

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

  /** Create a `Qty` with the specified value, keeping the runtime represantation of the units. */
  def withValue[N](value: N): Qty[N] = Qty[N](value, self)

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

  implicit class TaggedUnitTypeOps[Tag](val unitType: UnitType @@ Tag) extends AnyVal {

    /**
     * Create a `Qty` with the specified value, keeping the runtime represantation of the units,
     * propagating the unit tag into the `Qty`.
     */
    def withValueTagged[N](value: N): Qty[N] @@ Tag =
      tag[Tag](Qty[N](value, unitType))
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
 * Type class placing a `Tag` on a unit of measure.
 */
class IsTaggedUnit[U, Tag](implicit ev: UnitOfMeasure[U]) extends IsTagged[UnitOfMeasure[U], Tag] {
  def unit: UnitOfMeasure[U] @@ Tag = tag[Tag](ev)
}
