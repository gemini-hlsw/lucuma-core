// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.kernel.laws.discipline.*
import lucuma.core.math.arb.*
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.arb.*
import monocle.law.discipline.*
import munit.*

final class GmosGratingConfigSuite extends DisciplineSuite {
  import ArbEnumerated.given
  import ArbWavelength.given
  import lucuma.core.model.sequence.gmos.arb.ArbGmosGratingConfig.given

  checkAll("Eq[GmosGratingConfig.North]", EqTests[GmosGratingConfig.North].eqv)
  checkAll("GmosGratingConfig.North.grating", LensTests(GmosGratingConfig.North.grating))
  checkAll("GmosGratingConfig.North.order", LensTests(GmosGratingConfig.North.order))
  checkAll("GmosGratingConfig.North.wavelength", LensTests(GmosGratingConfig.North.wavelength))

  checkAll("Eq[GmosGratingConfig.South]", EqTests[GmosGratingConfig.South].eqv)
  checkAll("GmosGratingConfig.South.grating", LensTests(GmosGratingConfig.South.grating))
  checkAll("GmosGratingConfig.South.order", LensTests(GmosGratingConfig.South.order))
  checkAll("GmosGratingConfig.South.wavelength", LensTests(GmosGratingConfig.South.wavelength))

  checkAll("Eq[GmosGratingConfig]", EqTests[GmosGratingConfig].eqv)
  checkAll("GmosGratingConfig.north", PrismTests(GmosGratingConfig.north))
  checkAll("GmosGratingConfig.south", PrismTests(GmosGratingConfig.south))
}
