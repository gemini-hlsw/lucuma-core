// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.StepTime
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Duration

trait ArbStepTime {
  import ArbTime._

  implicit val arbStepTime: Arbitrary[StepTime] = Arbitrary(
    for {
      configChange <- arbitrary[Duration]
      exposure     <- arbitrary[Duration]
      readout      <- arbitrary[Duration]
      write        <- arbitrary[Duration]
      total        <- arbitrary[Duration]
    } yield StepTime(configChange, exposure, readout, write, total)
  )

  implicit val cogStepTime: Cogen[StepTime] =
    Cogen[(Duration, Duration, Duration, Duration, Duration)].contramap(t =>
      (t.configChange, t.exposure, t.readout, t.write, t.total)
    )
}

object ArbStepTime extends ArbStepTime
