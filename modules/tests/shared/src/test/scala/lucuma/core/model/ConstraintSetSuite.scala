// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.arb._
import lucuma.core.model.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.GidTests
import monocle.law.discipline._
import munit._

final class ConstraintSetSuite extends DisciplineSuite {
  import ArbConstraintSet._
  import ArbElevationRange._
  import ArbEnumerated._
  import ArbGid._

  checkAll("Eq[ConstraintSet]", EqTests[ConstraintSet].eqv)
  checkAll("ConstraintSet.Id", GidTests[ConstraintSet.Id].gid)
  checkAll("ConstraintSet.name", LensTests(ConstraintSet.name))
  checkAll("ConstraintSet.imageQuality", LensTests(ConstraintSet.imageQuality))
  checkAll("ConstraintSet.cloudExtinction", LensTests(ConstraintSet.cloudExtinction))
  checkAll("ConstraintSet.skyBackground", LensTests(ConstraintSet.skyBackground))
  checkAll("ConstraintSet.waterVapor", LensTests(ConstraintSet.waterVapor))
  checkAll("ConstraintSet.elevationRange", LensTests(ConstraintSet.elevationRange))
  checkAll("ConstraintSet.airmass", OptionalTests(ConstraintSet.airmass))
  checkAll("ConstraintSet.hourAngle", OptionalTests(ConstraintSet.hourAngle))
}
