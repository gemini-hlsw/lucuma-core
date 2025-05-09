// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import cats.syntax.contravariant.*
import cats.syntax.option.*
import coulomb.*
import coulomb.syntax.*
import lucuma.core.syntax.display.*
import lucuma.core.util.*
import monocle.Focus
import monocle.Lens

import scala.annotation.targetName

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

  override def toString: String =
    s"Measure($value${error.map(e => f" ± $e").orEmpty} ${units.abbv})"
}

object Measure extends MeasureLowPriority {
  given eqMeasure[N: Eq]: Eq[Measure[N]] = Eq.by(x => (x.value, x.units, x.error))

  given displayMeasure[N: Display]: Display[Measure[N]] =
    Display.by(
      m =>
        s"${m.value.shortName}${m.error.map(e => f" ± ${e.shortName}").orEmpty} ${m.units.shortName}",
      m =>
        s"${m.value.longName}${m.error.map(e => f" ± ${e.longName}").orEmpty} ${m.units.longName}"
    )

  given displayTaggedMeasure[N: Display, T]: Display[Measure[N] Of T] =
    Display[Measure[N]].narrow

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

  extension[N, T](measure: Measure[N] Of T)
    @targetName("withError_of_t")
    def withError(error: N): Measure[N] Of T = Measure.errorTagged.replace(error.some)(measure)
    @targetName("exact_of_t")
    def exact: Measure[N] Of T               = Measure.errorTagged.replace(none)(measure)

  extension[N](measure: Measure[N])

    /** Add an error value. */
    @targetName("withError")
    def withError(error: N): Measure[N] = Measure.error.replace(error.some)(measure)

    /** Remove error value. */
    @targetName("exact")
    def exact: Measure[N] = Measure.error.replace(none)(measure)

    /** Display measure without the uncertainty */
    def displayWithoutError(implicit d: Display[N]): String =
      s"${measure.value.shortName} ${measure.units.shortName}"

}

private class MeasureLowPriority {
  given eqTaggedMeasure[N: Eq, T]: Eq[Measure[N] Of T] =
    Eq.by(x => (x.value, x.units, x.error))
}
