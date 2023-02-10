// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.arb._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class GmosFpuMaskSuite extends DisciplineSuite {
  import ArbGmosFpuMask._
  import ArbEnumerated._
  import ArbRefined.given

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
  checkAll("GmosFpuMask.Custom.filename", LensTests(GmosFpuMask.Custom.filename))
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
}
