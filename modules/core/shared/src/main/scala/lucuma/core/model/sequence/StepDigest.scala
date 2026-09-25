// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.derived.*
import cats.syntax.monoid.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import monocle.Focus
import monocle.Lens

/**
 * Number of steps of one kind (bias, dark, arc, flat, observing) and their
 * estimated time, broken down by charge class.
 */
case class StepDigest(
  count: NonNegInt,
  time:  CategorizedTime
) derives Eq:

  def add(stepTime: CategorizedTime): StepDigest =
    this |+| StepDigest(NonNegInt.unsafeFrom(1), stepTime)

object StepDigest:

  val Zero: StepDigest =
    StepDigest(NonNegInt.MinValue, CategorizedTime.Zero)

  /**
   * Both count and time saturate rather than overflow.
   */
  given Monoid[StepDigest] =
    Monoid.instance(
      Zero,
      (a, b) =>
        val count = (a.count.value.toLong + b.count.value).min(Int.MaxValue).toInt
        StepDigest(NonNegInt.unsafeFrom(count), a.time |+| b.time)
    )

  /** @group Optics */
  val count: Lens[StepDigest, NonNegInt] =
    Focus[StepDigest](_.count)

  /** @group Optics */
  val time: Lens[StepDigest, CategorizedTime] =
    Focus[StepDigest](_.time)
