// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package scorpio
package arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ScorpioVisReadoutMode
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbScorpioChannelConfig:

  import ArbEnumerated.given
  import ArbTimeSpan.given

  given Arbitrary[ScorpioChannelConfig.Vis] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
        m <- arbitrary[ScorpioVisReadoutMode]
      yield ScorpioChannelConfig.Vis(t, c, m)

  given Arbitrary[ScorpioChannelConfig.Ir] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
      yield ScorpioChannelConfig.Ir(t, c)

  given Arbitrary[ScorpioChannelConfig] =
    Arbitrary:
      Gen.oneOf(arbitrary[ScorpioChannelConfig.Vis], arbitrary[ScorpioChannelConfig.Ir])

  given Cogen[ScorpioChannelConfig.Vis] =
    Cogen[(TimeSpan, Int, ScorpioVisReadoutMode)].contramap: a =>
      (a.exposureTime, a.exposureCount.value, a.readoutMode)

  given Cogen[ScorpioChannelConfig.Ir] =
    Cogen[(TimeSpan, Int)].contramap: a =>
      (a.exposureTime, a.exposureCount.value)

  given Cogen[ScorpioChannelConfig] =
    Cogen[Either[ScorpioChannelConfig.Vis, ScorpioChannelConfig.Ir]].contramap:
      case v: ScorpioChannelConfig.Vis => Left(v)
      case i: ScorpioChannelConfig.Ir  => Right(i)

object ArbScorpioChannelConfig extends ArbScorpioChannelConfig
