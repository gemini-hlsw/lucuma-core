// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class ConstraintSetSuite extends DisciplineSuite {
  import ArbConstraintSet._
  import ArbElevationRange._
  import ArbEnumerated._
  import ArbRefined.given

  // Laws
  checkAll("Eq[ConstraintSet]", EqTests[ConstraintSet].eqv)

  // Optics
  checkAll("ConstraintSet.imageQuality", LensTests(ConstraintSet.imageQuality))
  checkAll("ConstraintSet.cloudExtinction", LensTests(ConstraintSet.cloudExtinction))
  checkAll("ConstraintSet.skyBackground", LensTests(ConstraintSet.skyBackground))
  checkAll("ConstraintSet.waterVapor", LensTests(ConstraintSet.waterVapor))
  checkAll("ConstraintSet.elevationRange", LensTests(ConstraintSet.elevationRange))

  checkAll("ConstraintSet.airMass", OptionalTests(ConstraintSet.airMass))
  checkAll("ConstraintSet.airMassMin", OptionalTests(ConstraintSet.airMassMin))
  checkAll("ConstraintSet.airMassMax", OptionalTests(ConstraintSet.airMassMax))

  checkAll("ConstraintSet.hourAngle", OptionalTests(ConstraintSet.hourAngle))
  checkAll("ConstraintSet.hourAngleMin", OptionalTests(ConstraintSet.hourAngleMin))
  checkAll("ConstraintSet.hourAngleMax", OptionalTests(ConstraintSet.hourAngleMax))
}
