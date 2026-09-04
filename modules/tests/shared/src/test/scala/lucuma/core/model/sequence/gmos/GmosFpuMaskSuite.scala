// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.kernel.laws.discipline.*
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.model.ToBeDefined
import lucuma.core.model.arb.ArbMaskDefinition
import lucuma.core.model.sequence.gmos.arb.*
import lucuma.core.util.arb.*
import monocle.law.discipline.*
import munit.*

final class GmosFpuMaskSuite extends DisciplineSuite {
  import ArbEnumerated.given
  import ArbGmosFpuMask.given
  import ArbMaskDefinition.given

  checkAll(
    "Eq[GmosFpuMask.Builtin[GmosNorthGrating]]",
    EqTests[GmosFpuMask.Builtin[GmosNorthGrating]].eqv
  )
  checkAll(
    "Eq[GmosFpuMask.Builtin[GmosSouthGrating]]",
    EqTests[GmosFpuMask.Builtin[GmosNorthGrating]].eqv
  )
  checkAll(
    "GmosFpuMask.Builtin.value[GmosNorthGrating]",
    IsoTests(GmosFpuMask.Builtin.value[GmosNorthGrating])
  )
  checkAll(
    "GmosFpuMask.Builtin.value[GmosSoutthGrating]",
    IsoTests(GmosFpuMask.Builtin.value[GmosSouthGrating])
  )

  checkAll("Eq[GmosFpuMask.Custom", EqTests[GmosFpuMask.Custom].eqv)
  checkAll("GmosFpuMask.Custom.maskDefinition", LensTests(GmosFpuMask.Custom.maskDefinition))
  checkAll("GmosFpuMask.Custom.slitWidth", LensTests(GmosFpuMask.Custom.slitWidth))

  checkAll("Eq[GmosFpuMask[GmosNorthGrating]]", EqTests[GmosFpuMask[GmosNorthGrating]].eqv)
  checkAll("Eq[GmosFpuMask[GmosSouthGrating]]", EqTests[GmosFpuMask[GmosSouthGrating]].eqv)
  checkAll("GmosFpuMask.builtin[GmosNorthGrating]",
           PrismTests(GmosFpuMask.builtin[GmosNorthGrating])
  )
  checkAll("GmosFpuMask.builtin[GmosSouthGrating]",
           PrismTests(GmosFpuMask.builtin[GmosSouthGrating])
  )
  checkAll("GmosFpuMask.custom[GmosNorthGrating]", PrismTests(GmosFpuMask.custom[GmosNorthGrating]))
  checkAll("GmosFpuMask.custom[GmosSouthGrating]", PrismTests(GmosFpuMask.custom[GmosSouthGrating]))

  test("GmosFpuMask.fold") {
    val b: GmosFpuMask[Int] = GmosFpuMask.Builtin(1)
    assertEquals(b.fold(_ => "builtin", _ => "custom"), "builtin")

    val c: GmosFpuMask[Int] = GmosFpuMask.Custom(ToBeDefined, GmosCustomSlitWidth.CustomWidth_0_25)
    assertEquals(c.fold(_ => "builtin", _ => "custom"), "custom")
  }
}
