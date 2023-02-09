// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import lucuma.core.optics.laws.discipline.SplitEpiTests
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class ElevationRangeSuite extends DisciplineSuite {
  import ArbElevationRange._
  import ArbEnumerated._
  import ArbRefined.given

  // Laws
  checkAll("Eq[ElevationRange.AirMass]", EqTests[ElevationRange.AirMass].eqv)
  checkAll("Eq[ElevationRange.HourAngle]", EqTests[ElevationRange.HourAngle].eqv)
  checkAll("Eq[ElevationRange]", EqTests[ElevationRange].eqv)

  // Optics
  checkAll("ElevationRange.AirMass.min", LensTests(ElevationRange.AirMass.min))
  checkAll("ElevationRange.AirMass.max", LensTests(ElevationRange.AirMass.max))
  checkAll("ElevationRange.AirMass.fromDecimalValues",
           SplitEpiTests(ElevationRange.AirMass.fromDecimalValues).splitEpi
  )
  checkAll("ElevationRange.AirMass.fromOrderedDecimalValues",
           PrismTests(ElevationRange.AirMass.fromOrderedDecimalValues)
  )

  checkAll("ElevationRange.HourAngle.minHours", LensTests(ElevationRange.HourAngle.minHours))
  checkAll("ElevationRange.HourAngle.maxHours", LensTests(ElevationRange.HourAngle.maxHours))
  checkAll("ElevationRange.HourAngle.fromDecimalHours",
           SplitEpiTests(ElevationRange.HourAngle.fromDecimalHours).splitEpi
  )
  checkAll("ElevationRange.HourAngle.fromOrderedDecimalHours",
           PrismTests(ElevationRange.HourAngle.fromOrderedDecimalHours)
  )

  checkAll("ElevationRange.airMass", PrismTests(ElevationRange.airMass))
  checkAll("ElevationRange.hourAngle", PrismTests(ElevationRange.hourAngle))
}
