// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.arb.ArbAngle
import lucuma.core.model.arb.ArbPosAngle
import monocle.law.discipline._
import munit.DisciplineSuite

class PosAngleSuite extends DisciplineSuite {

  import ArbAngle._
  import ArbPosAngle._

  checkAll("Eq[PosAngle]", EqTests[PosAngleConstraint].eqv)

  checkAll("Optional[PosAngle, Angle]", OptionalTests(PosAngleConstraint.angleOptional))
}
