// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.derived.*
import cats.syntax.monoid.*
import eu.timepit.refined.types.numeric.NonNegInt
import monocle.Focus
import monocle.Lens

/**
 * Calibrations needed over an observation's science time.
 *
 * @param existing calibrations already created, with time from their own
 *                 estimates; they are observations of their own and are not
 *                 charged to the science observation
 * @param expected calibrations predicted but not created yet
 */
case class CalibrationDigest(
  existing: CalibrationEstimate,
  expected: CalibrationEstimate
) derives Eq:

  /** Calibrations still needed over the science time, existing and expected. */
  def count: NonNegInt =
    (existing |+| expected).count

  /** Calibration time charged to the science observation. */
  def chargedTime: CategorizedTime =
    expected.time

object CalibrationDigest:

  val Zero: CalibrationDigest =
    CalibrationDigest(CalibrationEstimate.Zero, CalibrationEstimate.Zero)

  /** @group Optics */
  val existing: Lens[CalibrationDigest, CalibrationEstimate] =
    Focus[CalibrationDigest](_.existing)

  /** @group Optics */
  val expected: Lens[CalibrationDigest, CalibrationEstimate] =
    Focus[CalibrationDigest](_.expected)
