// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.arb.*
import monocle.law.discipline.*
import munit.*

final class StaticConfigSuite extends DisciplineSuite {
  import ArbEnumerated.*
  import lucuma.core.model.sequence.gmos.arb.ArbGmosNodAndShuffle.*
  import lucuma.core.model.sequence.gmos.arb.ArbStaticConfig.*

  checkAll("Eq[StaticConfig.GmosNorth]", EqTests[StaticConfig.GmosNorth].eqv)
  checkAll("StaticConfig.GmosNorth.stageMode", LensTests(StaticConfig.GmosNorth.stageMode))
  checkAll("StaticConfig.GmosNorth.detector", LensTests(StaticConfig.GmosNorth.detector))
  checkAll("StaticConfig.GmosNorth.mosPreImaging", LensTests(StaticConfig.GmosNorth.mosPreImaging))
  checkAll("StaticConfig.GmosNorth.nodAndShuffle", LensTests(StaticConfig.GmosNorth.nodAndShuffle))

  checkAll("Eq[StaticConfig.Gmos]", EqTests[StaticConfig.GmosSouth].eqv)
  checkAll("StaticConfig.GmosSouth.stageMode", LensTests(StaticConfig.GmosSouth.stageMode))
  checkAll("StaticConfig.GmosSouth.detector", LensTests(StaticConfig.GmosSouth.detector))
  checkAll("StaticConfig.GmosSouth.mosPreImaging", LensTests(StaticConfig.GmosSouth.mosPreImaging))
  checkAll("StaticConfig.GmosSouth.nodAndShuffle", LensTests(StaticConfig.GmosSouth.nodAndShuffle))

  checkAll("Eq[StaticConfig]", EqTests[StaticConfig].eqv)
  checkAll("StaticConfig.gmosNorth", PrismTests(StaticConfig.gmosNorth))
  checkAll("StaticConfig.gmosSouth", PrismTests(StaticConfig.gmosSouth))
}