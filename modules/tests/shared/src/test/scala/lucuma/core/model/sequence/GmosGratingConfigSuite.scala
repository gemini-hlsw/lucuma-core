// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class GmosGratingConfigSuite extends DisciplineSuite {
  import ArbGmosGratingConfig._
  import ArbEnumerated._
  import ArbWavelength._

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
