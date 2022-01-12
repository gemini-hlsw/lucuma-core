// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import cats.syntax.option._
import coulomb._
import lucuma.core.syntax.display._
import lucuma.core.util.Display
import monocle.Focus
import monocle.Lens
import shapeless.tag

/**
 * A magnitude of type `N` and a runtime representation of a physical unit.
 */
final case class Measure[N](value: N, units: Units, error: Option[N] = none) { self =>

  /**
   * Convert to `coulomb.Quantity`. Error is dropped.
   */
  def toExactQuantity: Quantity[N, units.Type] = value.withUnit[units.Type]

  /**
   * Convert error to `coulomb.Quantity`.
   */
  def valueQuantity: Option[Quantity[N, units.Type]] = error.map(_.withUnit[units.Type])

  protected[dimensional] def errStr = error.map(e => f" ± $e").orEmpty

  override def toString: String =
    s"Measure($value$errStr ${units.abbv})"
}

object Measure {
  implicit def eqMeasure[N: Eq]: Eq[Measure[N]] = Eq.by(x => (x.value, x.units, x.error))

  implicit def eqTaggedMeasure[N: Eq, T]: Eq[Measure[N] Of T] =
    Eq.by(x => (x.value, x.units, x.error))

  implicit def displayMeasure[N: Display]: Display[Measure[N]] =
    Display.by(
      m => s"${m.value.shortName}${m.errStr} ${m.units.abbv}",
      m => s"${m.value.longName}${m.errStr} ${m.units.name}"
    )

  /** @group Optics */
  def value[N]: Lens[Measure[N], N] = Focus[Measure[N]](_.value)

  /** @group Optics */
  def valueTagged[N, T]: Lens[Measure[N] Of T, N] =
    Lens[Measure[N] Of T, N](_.value) { v => q =>
      val tagged = tag[T](value.replace(v)(q))
      tagged
    }

  /** @group Optics */
  def units[N]: Lens[Measure[N], Units] = Focus[Measure[N]](_.units)

  /** @group Optics */
  def unitsTagged[N, T]: Lens[Measure[N] Of T, Units Of T] =
    Lens[Measure[N] Of T, Units Of T] { q =>
      val tagged = tag[T](q.units)
      tagged
    } { v => q =>
      val tagged = tag[T](units.replace(v)(q))
      tagged
    }

  /** @group Optics */
  def error[N]: Lens[Measure[N], Option[N]] = Focus[Measure[N]](_.error)

  /** @group Optics */
  def errorTagged[N, T]: Lens[Measure[N] Of T, Option[N]] =
    Lens[Measure[N] Of T, Option[N]](_.error) { v => q =>
      val tagged = tag[T](error.replace(v)(q))
      tagged
    }

  implicit class TaggedMeasureOps[N, T](val measure: Measure[N] Of T) extends AnyVal {
    def withError(error: N): Measure[N] Of T = Measure.errorTagged.replace(error.some)(measure)
    def exact: Measure[N] Of T               = Measure.errorTagged.replace(none)(measure)
  }

  implicit class MeasureOps[N](val measure: Measure[N]) extends AnyVal {

    /** Add an error value. */
    def withError(error: N): Measure[N] = Measure.error.replace(error.some)(measure)

    /** Remove error value. */
    def exact: Measure[N] = Measure.error.replace(none)(measure)
  }

}
