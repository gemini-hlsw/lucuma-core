// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.derived.*
import cats.syntax.monoid.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * Number of GCAL steps of a given lamp type and their estimated total time.
 */
case class GcalDigest(
  count: NonNegInt,
  time:  TimeSpan
) derives Eq:

  def add(stepTime: TimeSpan): GcalDigest =
    this |+| GcalDigest(NonNegInt.unsafeFrom(1), stepTime)

object GcalDigest:

  val Zero: GcalDigest =
    GcalDigest(NonNegInt.MinValue, TimeSpan.Zero)

  /**
   * Both count and time saturate rather than overflow.
   */
  given Monoid[GcalDigest] =
    Monoid.instance(
      Zero,
      (a, b) =>
        val count = (a.count.value.toLong + b.count.value).min(Int.MaxValue).toInt
        GcalDigest(NonNegInt.unsafeFrom(count), a.time |+| b.time)
    )

  /** @group Optics */
  val count: Lens[GcalDigest, NonNegInt] =
    Focus[GcalDigest](_.count)

  /** @group Optics */
  val time: Lens[GcalDigest, TimeSpan] =
    Focus[GcalDigest](_.time)
