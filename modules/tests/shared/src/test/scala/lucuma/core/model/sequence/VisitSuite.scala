// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.UidTests
import monocle.law.discipline._
import munit._
import org.scalacheck.Test
import org.typelevel.cats.time._

final class VisitSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbUid._
  import ArbVisit._
  import ArbStaticConfig._
  import ArbStepRecord._
  import ArbTime._

  // Limit steps per Visit
  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Visit.Id", UidTests[Visit.Id].uid)

  checkAll("Eq[VisitSuite.GmosNorth]", EqTests[Visit.GmosNorth].eqv)
  checkAll("Visit.GmosNorth.id", LensTests(Visit.GmosNorth.id))
  checkAll("Visit.GmosNorth.created", LensTests(Visit.GmosNorth.created))
  checkAll("Visit.GmosNorth.staticConfig", LensTests(Visit.GmosNorth.staticConfig))
  checkAll("Visit.GmosNorth.steps", LensTests(Visit.GmosNorth.steps))

  checkAll("Eq[VisitSuite.GmosSouth]", EqTests[Visit.GmosSouth].eqv)
  checkAll("Visit.GmosSouth.id", LensTests(Visit.GmosSouth.id))
  checkAll("Visit.GmosSouth.created", LensTests(Visit.GmosSouth.created))
  checkAll("Visit.GmosSouth.staticConfig", LensTests(Visit.GmosSouth.staticConfig))
  checkAll("Visit.GmosSouth.steps", LensTests(Visit.GmosSouth.steps))

  checkAll("Eq[Visit]", EqTests[Visit].eqv)
  checkAll("Visit.GmosNorth", PrismTests(Visit.gmosNorth))
  checkAll("Visit.GmosSouth", PrismTests(Visit.gmosSouth))
}
