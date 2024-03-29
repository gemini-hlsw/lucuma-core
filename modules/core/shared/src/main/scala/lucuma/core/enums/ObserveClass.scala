// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.Monoid
import cats.syntax.order.*
import lucuma.core.util.Enumerated

/**
 * The class of an individual observe and (considering all the observes in an
 * observation) of the observation itself.  The observe classes are listed in
 * priority order.  For example, if one step is a `ProgramCal` and another is
 * a `Science` step, the atom that contains them both is considered `Science`.
 */
enum ObserveClass(
  val tag:          String,
  val name:         String,
  val abbreviation: String,

  /**
   * Which entity (if any) is charged for the time associated with the
   * observe's dataset(s).
   */
  val chargeClass:  ChargeClass,

  /**
   * Are the datasets associated with this observe considered to be
   * calibration datasets.
   */
  val isCalibration: Boolean,

  /**
   * Whether the datasets produced by the corresponding observe respect the
   * overall proprietary period (or are instead immediately made public).
   */
  val respectsProprietaryPeriod: Boolean

) derives Enumerated {

  case Science extends ObserveClass(
    "science",
    "Science",
    "SCI",
    ChargeClass.Program,
    isCalibration             = false,
    respectsProprietaryPeriod = true
  )

  case ProgramCal extends ObserveClass(
    "programCal",
    "Nighttime Program Calibration",
    "NCAL",
    ChargeClass.Program,
    isCalibration             = true,
    respectsProprietaryPeriod = false
  )

  case PartnerCal extends ObserveClass(
    "partnerCal",
    "Nighttime Partner Calibration",
    "PCAL",
    ChargeClass.Partner,
    isCalibration             = true,
    respectsProprietaryPeriod = false
  )

  case Acquisition extends ObserveClass(
    "acquisition",
    "Acquisition",
    "ACQ",
    ChargeClass.Program,
    isCalibration             = false,
    respectsProprietaryPeriod = true
  )

  case AcquisitionCal extends ObserveClass(
    "acquisitionCal",
    "Acquisition Calibration",
    "ACAL",
    ChargeClass.Partner,
    isCalibration             = true,
    respectsProprietaryPeriod = false
  )

  case DayCal extends ObserveClass(
    "dayCal",
    "Daytime Calibration",
    "DCAL",
    ChargeClass.NonCharged,
    isCalibration             = true,
    respectsProprietaryPeriod = false
  )

}

object ObserveClass {

  given Monoid[ObserveClass] with {
    def empty: ObserveClass =
      ObserveClass.DayCal

    def combine(c0: ObserveClass, c1: ObserveClass): ObserveClass =
      c0 min c1
  }

}