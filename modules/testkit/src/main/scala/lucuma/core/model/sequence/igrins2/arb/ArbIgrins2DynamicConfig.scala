// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2.arb

import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbIgrins2DynamicConfig:

  given Arbitrary[Igrins2DynamicConfig] = Arbitrary(
    for
      exposure <- arbitrary[TimeSpan]
    yield Igrins2DynamicConfig(exposure)
  )

  given Cogen[Igrins2DynamicConfig] =
    Cogen[TimeSpan].contramap(_.exposure)

object ArbIgrins2DynamicConfig extends ArbIgrins2DynamicConfig
