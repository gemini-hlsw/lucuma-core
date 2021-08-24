// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.model.arb._
import lucuma.core.model.SpatialProfile.GaussianSource
import munit._
import lucuma.core.optics.laws.discipline.SplitMonoTests

final class SpatialProfileSuite extends DisciplineSuite {
  import ArbSpatialProfile._

  // Laws
  checkAll("Eq[SpatialProfile]", EqTests[SpatialProfile].eqv)

  // Optics
  checkAll("GaussianSource.arcsec", SplitMonoTests(GaussianSource.arcsec).splitMono)

}
