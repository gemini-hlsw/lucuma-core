// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.ArbRefined
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbElevationRange {
  import ArbRefined.given

  given Arbitrary[ElevationRange.AirMass] =
    Arbitrary {
      for {
        min <- arbitrary[ElevationRange.AirMass.DecimalValue]
        max <- arbitrary[ElevationRange.AirMass.DecimalValue]
      } yield ElevationRange.AirMass.fromDecimalValues.get((min, max))
    }

  given Cogen[ElevationRange.AirMass] =
    Cogen[(ElevationRange.AirMass.DecimalValue, ElevationRange.AirMass.DecimalValue)]
      .contramap(t => (t.min, t.max))

  given Arbitrary[ElevationRange.HourAngle] =
    Arbitrary {
      for {
        min <- arbitrary[ElevationRange.HourAngle.DecimalHour]
        max <- arbitrary[ElevationRange.HourAngle.DecimalHour]
      } yield ElevationRange.HourAngle.fromDecimalHours.get((min, max))
    }

  given Cogen[ElevationRange.HourAngle] =
    Cogen[(ElevationRange.HourAngle.DecimalHour, ElevationRange.HourAngle.DecimalHour)]
      .contramap(t => (t.minHours, t.maxHours))

  given Arbitrary[ElevationRange] =
    Arbitrary(
      Gen.oneOf(arbitrary[ElevationRange.AirMass], arbitrary[ElevationRange.HourAngle])
    )

  given Cogen[ElevationRange] =
    Cogen[Either[ElevationRange.AirMass, ElevationRange.HourAngle]].contramap(_ match {
      case am @ ElevationRange.AirMass(_, _)   => Left(am)
      case ha @ ElevationRange.HourAngle(_, _) => Right(ha)
    })
}

object ArbElevationRange extends ArbElevationRange
