// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.util.arb.ArbNewType.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*


trait ArbElevationRange {

  given Arbitrary[ElevationRange.ByAirMass] =
    Arbitrary {
      for {
        min <- arbitrary[AirMassConstraint]
        max <- arbitrary[AirMassConstraint]
      } yield ElevationRange.ByAirMass.FromAirMassConstraints.get((min, max))
    }

  given Cogen[ElevationRange.ByAirMass] =
    Cogen[(AirMassConstraint, AirMassConstraint)]
      .contramap(t => (t.min, t.max))

  given Arbitrary[ElevationRange.ByHourAngle] =
    Arbitrary {
      for {
        min <- arbitrary[HourAngleConstraint]
        max <- arbitrary[HourAngleConstraint]
      } yield ElevationRange.ByHourAngle.FromHourAngleConstraints.get((min, max))
    }

  given Cogen[ElevationRange.ByHourAngle] =
    Cogen[(HourAngleConstraint, HourAngleConstraint)]
      .contramap(t => (t.minHours, t.maxHours))

  given Arbitrary[ElevationRange] =
    Arbitrary(
      Gen.oneOf(arbitrary[ElevationRange.ByAirMass], arbitrary[ElevationRange.ByHourAngle])
    )

  given Cogen[ElevationRange] =
    Cogen[Either[ElevationRange.ByAirMass, ElevationRange.ByHourAngle]].contramap(_ match {
      case am @ ElevationRange.ByAirMass(_, _)   => Left(am)
      case ha @ ElevationRange.ByHourAngle(_, _) => Right(ha)
    })
}

object ArbElevationRange extends ArbElevationRange
