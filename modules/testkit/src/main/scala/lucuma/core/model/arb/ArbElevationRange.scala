// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.math.arb.ArbRefined
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbElevationRange {
  import ArbEnumerated._
  import ArbRefined._

  implicit val arbAirMassRange: Arbitrary[ElevationRange.AirMass] =
    Arbitrary {
      for {
        min <- arbitrary[ElevationRange.AirMass.DecimalValue]
        max <- arbitrary[ElevationRange.AirMass.DecimalValue]
      } yield ElevationRange.AirMass.fromDecimalValues.get((min, max))
    }

  implicit val cogAirMassRange: Cogen[ElevationRange.AirMass] =
    Cogen[(ElevationRange.AirMass.DecimalValue, ElevationRange.AirMass.DecimalValue)]
      .contramap(t => (t.min, t.max))

  implicit val arbHourAngleRange: Arbitrary[ElevationRange.HourAngle] =
    Arbitrary {
      for {
        min <- arbitrary[ElevationRange.HourAngle.DecimalHour]
        max <- arbitrary[ElevationRange.HourAngle.DecimalHour]
      } yield ElevationRange.HourAngle.fromDecimalHours.get((min, max))
    }

  implicit val cogHourAngleRange: Cogen[ElevationRange.HourAngle] =
    Cogen[(ElevationRange.HourAngle.DecimalHour, ElevationRange.HourAngle.DecimalHour)]
      .contramap(t => (t.minHours, t.maxHours))

  implicit val arbElevationRange: Arbitrary[ElevationRange] =
    Arbitrary(
      Gen.oneOf(arbitrary[ElevationRange.AirMass], arbitrary[ElevationRange.HourAngle])
    )

  implicit val cogElevationRange: Cogen[ElevationRange] =
    Cogen[Either[ElevationRange.AirMass, ElevationRange.HourAngle]].contramap(_ match {
      case am @ ElevationRange.AirMass(_, _)   => Left(am)
      case ha @ ElevationRange.HourAngle(_, _) => Right(ha)
    })
}

object ArbElevationRange extends ArbElevationRange
