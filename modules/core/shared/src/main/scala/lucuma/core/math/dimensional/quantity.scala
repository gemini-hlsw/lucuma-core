// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import _root_.cats.kernel.Eq
import coulomb._
// import monocle.Focus
import monocle.Lens
import shapeless.tag
import shapeless.tag.@@

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

  def value[N]: Lens[Qty[N], N] = Lens[Qty[N], N](_.value)(v =>
    q =>
      new Qty[N] {
        val value = v
        val unit  = q.unit
      }
  )

  def valueT[N, Tag]: Lens[Qty[N] @@ Tag, N] = Lens[Qty[N] @@ Tag, N](_.value)(v =>
    q =>
      tag[Tag](new Qty[N] {
        val value = v
        val unit  = q.unit
      })
  )

  def unit[N]: Lens[Qty[N], UnitType] = Lens[Qty[N], UnitType](_.unit)(v =>
    q =>
      new Qty[N] {
        val value = q.value
        val unit  = v
      }
  )

  def unitT[N, Tag]: Lens[Qty[N] @@ Tag, UnitType @@ Tag] =
    Lens[Qty[N] @@ Tag, UnitType @@ Tag](q => tag[Tag](q.unit))(v =>
      q =>
        tag[Tag](new Qty[N] {
          val value = q.value
          val unit  = v
        })
    )
}
