// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._
import org.typelevel.cats.time._

final class DynamicConfigSuite extends DisciplineSuite {
  import ArbDynamicConfig._
  import ArbEnumerated._
  import ArbInterval.given
  import ArbGmosCcdMode._
  import ArbGmosGratingConfig._
  import ArbGmosFpuMask._
  import ArbTime._

  checkAll("Eq[DynamicConfig.GmosNorth]", EqTests[DynamicConfig.GmosNorth].eqv)
  checkAll("DynamicConfig.GmosNorth.exposure", LensTests(DynamicConfig.GmosNorth.exposure))
  checkAll("DynamicConfig.GmosNorth.readout", LensTests(DynamicConfig.GmosNorth.readout))
  checkAll("DynamicConfig.GmosNorth.dtax", LensTests(DynamicConfig.GmosNorth.dtax))
  checkAll("DynamicConfig.GmosNorth.roi", LensTests(DynamicConfig.GmosNorth.roi))
  checkAll("DynamicConfig.GmosNorth.gratingConfig",
           LensTests(DynamicConfig.GmosNorth.gratingConfig)
  )
  checkAll("DynamicConfig.GmosNorth.filter", LensTests(DynamicConfig.GmosNorth.filter))
  checkAll("DynamicConfig.GmosNorth.fpu", LensTests(DynamicConfig.GmosNorth.fpu))

  checkAll("Eq[DynamicConfig.GmosSouth]", EqTests[DynamicConfig.GmosSouth].eqv)
  checkAll("DynamicConfig.GmosSouth.exposure", LensTests(DynamicConfig.GmosSouth.exposure))
  checkAll("DynamicConfig.GmosSouth.readout", LensTests(DynamicConfig.GmosSouth.readout))
  checkAll("DynamicConfig.GmosSouth.dtax", LensTests(DynamicConfig.GmosSouth.dtax))
  checkAll("DynamicConfig.GmosSouth.roi", LensTests(DynamicConfig.GmosSouth.roi))
  checkAll("DynamicConfig.GmosSouth.gratingConfig",
           LensTests(DynamicConfig.GmosSouth.gratingConfig)
  )
  checkAll("DynamicConfig.GmosSouth.filter", LensTests(DynamicConfig.GmosSouth.filter))
  checkAll("DynamicConfig.GmosSouth.fpu", LensTests(DynamicConfig.GmosSouth.fpu))

  checkAll("Eq[DynamicConfig]", EqTests[DynamicConfig].eqv)
  checkAll("DynamicConfig.gmosNorth", PrismTests(DynamicConfig.gmosNorth))
  checkAll("DynamicConfig.gmosSouth", PrismTests(DynamicConfig.gmosSouth))
}
