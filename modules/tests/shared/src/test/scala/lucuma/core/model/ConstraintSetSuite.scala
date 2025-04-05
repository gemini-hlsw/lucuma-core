// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.*
import lucuma.core.model.arb.*
import lucuma.core.util.arb.*
import monocle.law.discipline.*
import munit.*

final class ConstraintSetSuite extends DisciplineSuite {
  import ArbConstraintSet.given
  import ArbElevationRange.given
  import ArbEnumerated.given
  import ArbNewType.given
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
