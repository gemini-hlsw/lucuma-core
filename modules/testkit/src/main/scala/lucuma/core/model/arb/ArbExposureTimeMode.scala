// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonNegInt
import eu.timepit.refined.types.all.PosBigDecimal
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.NonNegDuration
import lucuma.core.model.arb.ArbNonNegDuration
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbExposureTimeMode {
  import ExposureTimeMode._
  import ArbNonNegDuration._
  import ArbRefined._

  implicit val arbSignalToNoise: Arbitrary[SignalToNoise] =
    Arbitrary {
      arbitrary[PosBigDecimal].map(SignalToNoise(_))
    }

  implicit val cogSignalToNoise: Cogen[SignalToNoise] =
    Cogen[PosBigDecimal].contramap(_.value)

  implicit val arbFixedExposure: Arbitrary[FixedExposure] =
    Arbitrary {
      for {
        c <- arbitrary[NonNegInt]
        t <- arbitrary[NonNegDuration]
      } yield FixedExposure(c, t)
    }

  implicit val cogFixedExposure: Cogen[FixedExposure] =
    Cogen[
      (
        NonNegInt,
        NonNegDuration
      )
    ].contramap { in =>
      (
        in.count,
        in.time
      )
    }

  implicit val arbExposureMode: Arbitrary[ExposureTimeMode] =
    Arbitrary {
      Gen.oneOf(arbitrary[FixedExposure], arbitrary[FixedExposure])
    }

  implicit val cogExposureMode: Cogen[ExposureTimeMode] =
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
