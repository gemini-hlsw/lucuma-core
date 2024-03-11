// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.enums.*
import lucuma.core.model.ProbeGuide
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbProbeGuide {

  given Arbitrary[ProbeGuide] =
    Arbitrary {
      for {
        f <- arbitrary[GuideProbe]
        t <- arbitrary[GuideProbe]
      } yield ProbeGuide(f, t)
    }

  given Cogen[ProbeGuide] =
    Cogen[(GuideProbe, GuideProbe)]
      .contramap(x => (x.from, x.to))
}

object ArbProbeGuide extends ArbProbeGuide
