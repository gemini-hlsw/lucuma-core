// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._
import org.scalacheck.Test
import org.typelevel.cats.time._

final class ManualConfigSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbUid._
  import ArbSequence._
  import ArbManualConfig._
  import ArbStaticConfig._
  import ArbTime._

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Eq[ManualConfig.GmosNorth]", EqTests[ManualConfig.GmosNorth].eqv)
  checkAll("ManualConfig.GmosNorth.static", LensTests(ManualConfig.GmosNorth.static))
  checkAll("ManualConfig.GmosNorth.setupTime", LensTests(ManualConfig.GmosNorth.setupTime))
  checkAll("ManualConfig.GmosNorth.acquisition", LensTests(ManualConfig.GmosNorth.acquisition))
  checkAll("ManualConfig.GmosNorth.science", LensTests(ManualConfig.GmosNorth.science))

  checkAll("Eq[ManualConfig.GmosSouth]", EqTests[ManualConfig.GmosSouth].eqv)
  checkAll("ManualConfig.GmosSouth.static", LensTests(ManualConfig.GmosSouth.static))
  checkAll("ManualConfig.GmosSouth.setupTime", LensTests(ManualConfig.GmosSouth.setupTime))
  checkAll("ManualConfig.GmosSouth.acquisition", LensTests(ManualConfig.GmosSouth.acquisition))
  checkAll("ManualConfig.GmosSouth.science", LensTests(ManualConfig.GmosSouth.science))

  checkAll("Eq[ManualConfig]", EqTests[ManualConfig].eqv)
  checkAll("ManualConfig.gmosNorth", PrismTests(ManualConfig.gmosNorth))
  checkAll("ManualConfig.gmosSouth", PrismTests(ManualConfig.gmosSouth))
}
