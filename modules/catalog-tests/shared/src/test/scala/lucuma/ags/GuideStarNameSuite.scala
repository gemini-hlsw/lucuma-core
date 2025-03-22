// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.kernel.Eq
import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats.*
import io.circe.*
import io.circe.refined.*
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.ags.arb.ArbGuideStarName.given
import monocle.law.discipline.PrismTests

class GuideStarNameSuite extends munit.DisciplineSuite {
  test("typeclasses") {
    checkAll("GuideStarName", EqTests[GuideStarName].eqv)
    checkAll("GuideStarNameCodec", CodecTests[GuideStarName].codec)
    checkAll("GuideStarName.from", PrismTests(GuideStarName.from))
    checkAll("GuideStarName.gaiaSourceId", PrismTests(GuideStarName.gaiaSourceId))
  }

}
