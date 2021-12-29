// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import coulomb._
import monocle.Focus
import monocle.Lens
import shapeless.tag

/**
 * A magnitude of type `N` and a runtime representation of a physical unit.
 */
final case class Measure[N](value: N, units: Units) {

  /**
   * Convert to `coulomb.Quantity`.
   */
  def toQuantity: Quantity[N, units.Type] = value.withUnit[units.Type]
}

object Measure {
  implicit def eqMeasure[N: Eq]: Eq[Measure[N]] = Eq.by(x => (x.value, x.units))

  implicit def eqTaggedMeasure[N: Eq, Tag]: Eq[Measure[N] Of Tag] = Eq.by(x => (x.value, x.units))

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
}
