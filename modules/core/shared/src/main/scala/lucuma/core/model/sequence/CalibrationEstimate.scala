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
 * Number of calibration observations and their estimated time, broken down
 * by charge class.
 */
case class CalibrationEstimate(
  count: NonNegInt,
  time:  CategorizedTime
) derives Eq

object CalibrationEstimate:

  val Zero: CalibrationEstimate =
    CalibrationEstimate(NonNegInt.MinValue, CategorizedTime.Zero)

  /**
   * Both count and time saturate rather than overflow.
   */
  given Monoid[CalibrationEstimate] =
    Monoid.instance(
      Zero,
      (a, b) =>
        val count = (a.count.value.toLong + b.count.value).min(Int.MaxValue).toInt
        CalibrationEstimate(NonNegInt.unsafeFrom(count), a.time |+| b.time)
    )

  /** @group Optics */
  val count: Lens[CalibrationEstimate, NonNegInt] =
    Focus[CalibrationEstimate](_.count)

  /** @group Optics */
  val time: Lens[CalibrationEstimate, CategorizedTime] =
    Focus[CalibrationEstimate](_.time)
