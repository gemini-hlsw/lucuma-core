// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.*
import lucuma.core.model.arb.*
import lucuma.core.optics.laws.discipline.SplitEpiTests
import lucuma.core.util.arb.*
import monocle.law.discipline.*
import munit.*

final class ElevationRangeSuite extends DisciplineSuite {
  import ArbElevationRange.given
  import ArbRefined.given
  import ArbNewType.given

  // Laws
  checkAll("Eq[ElevationRange.AirMass]", EqTests[ElevationRange.ByAirMass].eqv)
  checkAll("Eq[ElevationRange.HourAngle]", EqTests[ElevationRange.ByHourAngle].eqv)
  checkAll("Eq[ElevationRange]", EqTests[ElevationRange].eqv)

  // Optics
  checkAll("ElevationRange.AirMass.min", LensTests(ElevationRange.ByAirMass.min))
  checkAll("ElevationRange.AirMass.max", LensTests(ElevationRange.ByAirMass.max))
  checkAll(
    "ElevationRange.AirMass.FromBounds",
    SplitEpiTests(ElevationRange.ByAirMass.FromBounds).splitEpi
  )
  checkAll(
    "ElevationRange.AirMass.FromOrderedBounds",
    PrismTests(ElevationRange.ByAirMass.FromOrderedBounds)
  )

  checkAll("ElevationRange.HourAngle.minHours", LensTests(ElevationRange.ByHourAngle.minHours))
  checkAll("ElevationRange.HourAngle.maxHours", LensTests(ElevationRange.ByHourAngle.maxHours))
  checkAll(
    "ElevationRange.HourAngle.FromBounds",
    SplitEpiTests(ElevationRange.ByHourAngle.FromBounds).splitEpi
  )
  checkAll(
    "ElevationRange.HourAngle.FromOrderedHourAngleBounds",
    PrismTests(ElevationRange.ByHourAngle.FromOrderedBounds)
  )

  checkAll("ElevationRange.airMass", PrismTests(ElevationRange.airMass))
  checkAll("ElevationRange.hourAngle", PrismTests(ElevationRange.hourAngle))
}
