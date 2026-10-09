// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package scorpio
package arb

import lucuma.core.enums.ScorpioAdc
import lucuma.core.enums.ScorpioChannelSet
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbScorpioStaticConfig:

  import ArbEnumerated.given
  import ArbTimeSpan.given

  given Arbitrary[ScorpioStaticConfig] =
    Arbitrary:
      for
        c <- arbitrary[ScorpioChannelSet]
        a <- arbitrary[ScorpioAdc]
        s <- arbitrary[Option[TimeSpan]]
      yield ScorpioStaticConfig(c, a, s)

  given Cogen[ScorpioStaticConfig] =
    Cogen[(
      ScorpioChannelSet,
      ScorpioAdc,
      Option[TimeSpan]
    )].contramap: a =>
      (
        a.channelSet,
        a.adc,
        a.svcExposureTime
      )

object ArbScorpioStaticConfig extends ArbScorpioStaticConfig
