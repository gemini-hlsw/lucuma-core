// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import monocle.law.discipline._
import munit._

final class GmosGratingSuite extends DisciplineSuite {
  import ArbGmosGrating._
  import ArbEnumerated._
  import ArbWavelength._

  checkAll("Eq[GmosGrating.North]", EqTests[GmosGrating.North].eqv)
  checkAll("GmosGrating.North.grating", LensTests(GmosGrating.North.grating))
  checkAll("GmosGrating.North.order", LensTests(GmosGrating.North.order))
  checkAll("GmosGrating.North.wavelength", LensTests(GmosGrating.North.wavelength))

  checkAll("Eq[GmosGrating.South]", EqTests[GmosGrating.South].eqv)
  checkAll("GmosGrating.South.grating", LensTests(GmosGrating.South.grating))
  checkAll("GmosGrating.South.order", LensTests(GmosGrating.South.order))
  checkAll("GmosGrating.South.wavelength", LensTests(GmosGrating.South.wavelength))

  checkAll("Eq[GmosGrating]", EqTests[GmosGrating].eqv)
  checkAll("GmosGrating.north", PrismTests(GmosGrating.north))
  checkAll("GmosGrating.south", PrismTests(GmosGrating.south))
}
