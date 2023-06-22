// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.*
import lucuma.core.model.arb.*
import lucuma.core.optics.laws.discipline.SplitEpiTests
import monocle.law.discipline.*
import munit.*

final class ElevationRangeSuite extends DisciplineSuite {
  import ArbElevationRange.*
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
