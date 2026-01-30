// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.kernel.laws.discipline.*
import lucuma.core.enums.Igrins2FowlerSamples
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig.given
import lucuma.core.syntax.timespan.*
import munit.*

class Igrins2DynamicConfigSuite extends DisciplineSuite:
  checkAll("Eq[Igrins2DynamicConfig]", EqTests[Igrins2DynamicConfig].eqv)

  test("fowlerSamples from exposure time"):
    assertEquals(Igrins2DynamicConfig(1000.msTimeSpan).fowlerSamples, Igrins2FowlerSamples.One)
    assertEquals(Igrins2DynamicConfig(5.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Two)
    assertEquals(Igrins2DynamicConfig(10.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Four)
    assertEquals(Igrins2DynamicConfig(15.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Eight)
    assertEquals(Igrins2DynamicConfig(30.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Sixteen)
    assertEquals(Igrins2DynamicConfig(120.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Sixteen)
