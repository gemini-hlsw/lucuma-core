// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.kernel.laws.discipline.*
import lucuma.ags.arb.*
import lucuma.core.model.arb.*
import lucuma.core.optics.laws.discipline.SplitEpiTests
import munit.*

class GuideStarCandidateSuite extends DisciplineSuite {
  import ArbGuideStarCandidate.given
  import ArbTarget.given

  // Laws
  checkAll("Eq[GuideStarCandidate]", EqTests[GuideStarCandidate].eqv)
  // optics
  checkAll("GuideStarCandidate.siderealTarget",
           SplitEpiTests(GuideStarCandidate.siderealTarget).splitEpi
  )
}
