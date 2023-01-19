// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.StepTime
import lucuma.core.util.Interval
import lucuma.core.util.arb.ArbInterval
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen


trait ArbStepTime {
  import ArbInterval.given
  import ArbTime._

  implicit val arbStepTime: Arbitrary[StepTime] = Arbitrary(
    for {
      configChange <- arbitrary[Interval]
      exposure     <- arbitrary[Interval]
      readout      <- arbitrary[Interval]
      write        <- arbitrary[Interval]
      total        <- arbitrary[Interval]
    } yield StepTime(configChange, exposure, readout, write, total)
  )

  implicit val cogStepTime: Cogen[StepTime] =
    Cogen[(Interval, Interval, Interval, Interval, Interval)].contramap(t =>
      (t.configChange, t.exposure, t.readout, t.write, t.total)
    )
}

object ArbStepTime extends ArbStepTime
