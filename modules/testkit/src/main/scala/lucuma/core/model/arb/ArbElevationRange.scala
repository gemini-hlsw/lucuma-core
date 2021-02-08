// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.numeric.intervalClosedArbitrary
import lucuma.core.model.{ AirmassRange, ElevationRange, HourAngleRange }
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbElevationRange {
  import ArbEnumerated._

  // needed to prevent diverging implicits.
  implicit val arbIntDeciValue: Arbitrary[AirmassRange.IntDeciValue] = intervalClosedArbitrary
  implicit val arbIntDeciHour: Arbitrary[HourAngleRange.IntDeciHour] = intervalClosedArbitrary

  implicit val arbAirmassRange: Arbitrary[AirmassRange] =
    Arbitrary {
      for {
        min <- arbitrary[AirmassRange.IntDeciValue]
        max <- arbitrary[AirmassRange.IntDeciValue]
      } yield AirmassRange(min, max)
    }

  implicit val cogAirmassRange: Cogen[AirmassRange] =
    Cogen[(AirmassRange.IntDeciValue, AirmassRange.IntDeciValue)].contramap(amr =>
      (amr.deciMin, amr.deciMax)
    )

  implicit val arbHourAngleRange: Arbitrary[HourAngleRange] =
    Arbitrary {
      for {
        min <- arbitrary[HourAngleRange.IntDeciHour]
        max <- arbitrary[HourAngleRange.IntDeciHour]
      } yield HourAngleRange(min, max)
    }

  implicit val cogHourAngleRange: Cogen[HourAngleRange] =
    Cogen[(HourAngleRange.IntDeciHour, HourAngleRange.IntDeciHour)].contramap(har =>
      (har.deciMin, har.deciMax)
    )

  implicit val arbElevationRange: Arbitrary[ElevationRange] =
    Arbitrary {
      for {
        airmassRange   <- arbitrary[AirmassRange]
        hourAngleRange <- arbitrary[HourAngleRange]
        elevationRange <- Gen.oneOf(airmassRange, hourAngleRange)
      } yield elevationRange
    }

  implicit val cogElevationRange: Cogen[ElevationRange] =
    Cogen[Either[AirmassRange, HourAngleRange]].contramap {
      case airmass: AirmassRange     => Left(airmass)
      case hourAngle: HourAngleRange => Right(hourAngle)
    }
}

object ArbElevationRange extends ArbElevationRange
