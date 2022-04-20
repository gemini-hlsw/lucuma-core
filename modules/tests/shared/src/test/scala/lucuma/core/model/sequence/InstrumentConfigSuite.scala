// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._
import org.typelevel.cats.time._

final class InstrumentConfigSuite extends DisciplineSuite {
  import ArbInstrumentConfig._
  import ArbEnumerated._
  import ArbTime._
  import ArbGmosCcdMode._
  import ArbGmosGrating._
  import ArbGmosFpuMask._

  checkAll("Eq[InstrumentConfig.GmosNorth]", EqTests[InstrumentConfig.GmosNorth].eqv)
  checkAll("InstrumentConfig.GmosNorth.exposure", LensTests(InstrumentConfig.GmosNorth.exposure))
  checkAll("InstrumentConfig.GmosNorth.readout", LensTests(InstrumentConfig.GmosNorth.readout))
  checkAll("InstrumentConfig.GmosNorth.dtax", LensTests(InstrumentConfig.GmosNorth.dtax))
  checkAll("InstrumentConfig.GmosNorth.roi", LensTests(InstrumentConfig.GmosNorth.roi))
  checkAll("InstrumentConfig.GmosNorth.grating", LensTests(InstrumentConfig.GmosNorth.grating))
  checkAll("InstrumentConfig.GmosNorth.filter", LensTests(InstrumentConfig.GmosNorth.filter))
  checkAll("InstrumentConfig.GmosNorth.fpu", LensTests(InstrumentConfig.GmosNorth.fpu))

  checkAll("Eq[InstrumentConfig.GmosSouth]", EqTests[InstrumentConfig.GmosSouth].eqv)
  checkAll("InstrumentConfig.GmosSouth.exposure", LensTests(InstrumentConfig.GmosSouth.exposure))
  checkAll("InstrumentConfig.GmosSouth.readout", LensTests(InstrumentConfig.GmosSouth.readout))
  checkAll("InstrumentConfig.GmosSouth.dtax", LensTests(InstrumentConfig.GmosSouth.dtax))
  checkAll("InstrumentConfig.GmosSouth.roi", LensTests(InstrumentConfig.GmosSouth.roi))
  checkAll("InstrumentConfig.GmosSouth.grating", LensTests(InstrumentConfig.GmosSouth.grating))
  checkAll("InstrumentConfig.GmosSouth.filter", LensTests(InstrumentConfig.GmosSouth.filter))
  checkAll("InstrumentConfig.GmosSouth.fpu", LensTests(InstrumentConfig.GmosSouth.fpu))

  checkAll("Eq[InstrumentConfig]", EqTests[InstrumentConfig].eqv)
  checkAll("InstrumentConfig.gmosNorth", PrismTests(InstrumentConfig.gmosNorth))
  checkAll("InstrumentConfig.gmosSouth", PrismTests(InstrumentConfig.gmosSouth))
}
