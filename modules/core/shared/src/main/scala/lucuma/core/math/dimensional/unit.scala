// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import cats.syntax.all.*
import coulomb.ops.ShowUnit
import coulomb.ops.ShowUnitFull
import lucuma.core.util.*

import java.util.Objects

/**
 * Runtime representation of a physical unit. Wraps:
 *   - A type (usually phantom), meant to be `coulomb`'s type for the unit; and
 *   - A `UnitDefinition`, which is a common trait of `coulomb`'s `BaseUnit` and `DerivedUnit`.
 *
 * In `coulomb` units are types. However in some cases we want to have runtime representations of
 * units.
 */
trait Units { self =>
  type Type

  /** The full name of a unit, e.g. "squared meter" */
  def name: String

  /** The abbreviation of a unit, e.g. "m²" for "squared meter" */
  def abbv: String

  /**
   * The serialized form of a unit, e.g. "METER_2" for "squared meter"
   *
   * May be particularly useful in limited-charset codecs.
   *
   * The name `tag` was intentinally avoided to prevent confusion with the type-tags used in
   * `TaggedUnit` or `Units Of T`.
   */
  def serialized: String

  /**
   * Create a `Measure` with the specified value, keeping the runtime represantation of the units.
   */
  def withValue[N](value: N, error: Option[N] = none): Measure[N] = Measure[N](value, self, error)

  override def equals(obj: Any): Boolean = obj match {
    case that: Units => name == that.name && abbv == that.abbv && serialized == that.serialized
    case _           => false
  }

  override def hashCode: Int = Objects.hash(name, abbv, serialized)

  override def toString: String = abbv
}

object Units {
  given Eq[Units] = Eq.fromUniversalEquals

  given Display[Units] = Display.by(_.abbv, _.name)

  given displayTaggedUnits[T]: Display[Units Of T] =
    Display[Units].narrow

  extension[T](units: Units Of T)

    /**
     * Create a `Measure` with the specified value, keeping the runtime represantation of the units,
     * propagating the unit tag into the `Measure`.
     */
    def withValueTagged[N](value: N, error: Option[N] = none): Measure[N] Of T = {
      val tagged = tag[T](Measure[N](value, units, error))
      tagged
    }
}

/**
 * Type-parametrized runtime representation of physical unit `U`.
 *
 * Can be automatically derived if there's an implicit `BaseUnit[U]` or `DerivedUnit[U, _]` in
 * scope.
 */
final case class UnitOfMeasure[U](name: String, abbv: String, serialized: String) extends Units {
  type Type = U

  def withSerialized(serialized: String): UnitOfMeasure[U] = copy(serialized = serialized)
}

object UnitOfMeasure {
  def apply[U: UnitOfMeasure]: UnitOfMeasure[U] = summon[UnitOfMeasure[U]]

  given unitOfMeasureFromUnitString[U](using
    full: ShowUnitFull[U],
    abbv: ShowUnit[U],
    s:    TypeString[U]
  ): UnitOfMeasure[U] =
    UnitOfMeasure(full.value, abbv.value, s.serialized)

  given eqUnitOfMeasure[U]: Eq[UnitOfMeasure[U]] = Eq.fromUniversalEquals
}

/**
 * Typeclass placing a `Tag` on a unit of measure.
 */
class TaggedUnit[U, T](using ev: UnitOfMeasure[U]) {
  def unit: UnitOfMeasure[U] Of T = {
    val tagged = tag[T](ev)
    tagged
  }
}
