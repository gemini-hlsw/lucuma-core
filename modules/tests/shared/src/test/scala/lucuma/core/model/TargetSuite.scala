// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.implicits._
import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._

final class TargetSuite extends DisciplineSuite {
  import ArbTarget._

  // Laws
  checkAll("Target", OrderTests[Target].order)
  checkAll("TargetName", OrderTests[Target](Target.TargetNameOrder).order)
}
