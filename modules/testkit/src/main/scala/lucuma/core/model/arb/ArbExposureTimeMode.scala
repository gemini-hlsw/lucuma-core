// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.all.PosInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbExposureTimeMode {
  import ExposureTimeMode.*
  import ArbRefined.given
  import ArbSignalToNoise.given
  import ArbTimeSpan.given

  given Arbitrary[SignalToNoiseMode] =
    Arbitrary {
      arbitrary[SignalToNoise].map(SignalToNoiseMode(_))
    }

  given Cogen[SignalToNoiseMode] =
    Cogen[SignalToNoise].contramap(_.value)

  given Arbitrary[FixedExposureMode] =
    Arbitrary {
      for {
        c <- arbitrary[PosInt]
        t <- arbitrary[TimeSpan]
      } yield FixedExposureMode(c, t)
    }

  given Cogen[FixedExposureMode] =
    Cogen[
      (
        PosInt,
        TimeSpan
      )
    ].contramap { in =>
      (
        in.count,
        in.time
      )
    }

  given Arbitrary[ExposureTimeMode] =
    Arbitrary {
      Gen.oneOf(arbitrary[FixedExposureMode], arbitrary[FixedExposureMode])
    }

  given Cogen[ExposureTimeMode] =
    Cogen[
      (
        Option[SignalToNoiseMode],
        Option[FixedExposureMode]
      )
    ].contramap { in =>
      (
        ExposureTimeMode.signalToNoise.getOption(in),
        ExposureTimeMode.fixedExposure.getOption(in)
      )
    }
}

object ArbExposureTimeMode extends ArbExposureTimeMode
