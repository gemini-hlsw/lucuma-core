// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.optics.laws.discipline.SplitMonoTests
import munit._

class GaussianSourceSuite extends DisciplineSuite {
  import lucuma.core.model.arb.ArbGaussianSource._

  checkAll("Eq[GaussianSource]", EqTests[GaussianSource].eqv)
  checkAll("GaussianSource.arcsec", SplitMonoTests(GaussianSource.arcsec).splitMono)
}
