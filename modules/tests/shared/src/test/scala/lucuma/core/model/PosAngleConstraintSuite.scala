// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.arb.ArbAngle
import lucuma.core.model.arb.ArbPosAngleConstraint
import monocle.law.discipline.*
import munit.DisciplineSuite

class PosAngleConstraintSuite extends DisciplineSuite {

  import ArbAngle.given
  import ArbPosAngleConstraint.given

  checkAll("Eq[PosAngle]", EqTests[PosAngleConstraint].eqv)

  checkAll("Optional[PosAngle, Angle]", OptionalTests(PosAngleConstraint.angle))
}
