// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package scorpio
package arb

import lucuma.core.enums.ScorpioAdc
import lucuma.core.enums.ScorpioArmSet
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbScorpioStaticConfig:

  import ArbEnumerated.given
  import ArbScorpioNodAndShift.given
  import ArbTimeSpan.given

  given Arbitrary[ScorpioStaticConfig] =
    Arbitrary:
      for
        c <- arbitrary[ScorpioArmSet]
        a <- arbitrary[ScorpioAdc]
        s <- arbitrary[Option[TimeSpan]]
        n <- arbitrary[Option[ScorpioNodAndShift]]
      yield ScorpioStaticConfig(c, a, s, n)

  given Cogen[ScorpioStaticConfig] =
    Cogen[(
      ScorpioArmSet,
      ScorpioAdc,
      Option[TimeSpan],
      Option[ScorpioNodAndShift]
    )].contramap: a =>
      (
        a.armSet,
        a.adc,
        a.svcExposureTime,
        a.nodAndShift
      )

object ArbScorpioStaticConfig extends ArbScorpioStaticConfig
