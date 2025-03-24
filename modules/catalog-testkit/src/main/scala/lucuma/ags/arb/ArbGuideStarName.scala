// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags.arb

import lucuma.ags.GuideStarName
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGuideStarName:

  given Arbitrary[GuideStarName] =
    Arbitrary {
      Gen
        .frequency(
          // Get the occasional name with too many digits
          1  -> s"9${Long.MaxValue}",
          20 -> Arbitrary.arbLong.arbitrary
        )
        .map(a => GuideStarName.unsafeFrom(s"Gaia DR3 $a"))
    }

  given Cogen[GuideStarName] =
    Cogen[String].contramap(_.value.value)

object ArbGuideStarName extends ArbGuideStarName
