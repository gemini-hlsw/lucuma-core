// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.all.NonNegInt
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.NonNegDuration
import lucuma.core.model.arb.ArbNonNegDuration
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbExposureTimeMode {
  import ExposureTimeMode.*
  import ArbTimeSpan.given
  import ArbRefined.given

  given Arbitrary[SignalToNoise] =
    Arbitrary {
      arbitrary[PosBigDecimal].map(SignalToNoise(_))
    }

  given Cogen[SignalToNoise] =
    Cogen[PosBigDecimal].contramap(_.value)

  given Arbitrary[FixedExposure] =
    Arbitrary {
      for {
        c <- arbitrary[NonNegInt]
        t <- arbitrary[TimeSpan]
      } yield FixedExposure(c, t)
    }

  given Cogen[FixedExposure] =
    Cogen[
      (
        NonNegInt,
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
      Gen.oneOf(arbitrary[FixedExposure], arbitrary[FixedExposure])
    }

  given Cogen[ExposureTimeMode] =
    Cogen[
      (
        Option[SignalToNoise],
        Option[FixedExposure]
      )
    ].contramap { in =>
      (
        ExposureTimeMode.signalToNoise.getOption(in),
        ExposureTimeMode.fixedExposure.getOption(in)
      )
    }
}

object ArbExposureTimeMode extends ArbExposureTimeMode
