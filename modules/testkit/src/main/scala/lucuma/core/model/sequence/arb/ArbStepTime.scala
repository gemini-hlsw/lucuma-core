// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence.StepTime
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen


trait ArbStepTime {
  import ArbTimeSpan.given
  import ArbTime._

  implicit val arbStepTime: Arbitrary[StepTime] = Arbitrary(
    for {
      configChange <- arbitrary[TimeSpan]
      exposure     <- arbitrary[TimeSpan]
      readout      <- arbitrary[TimeSpan]
      write        <- arbitrary[TimeSpan]
      total        <- arbitrary[TimeSpan]
    } yield StepTime(configChange, exposure, readout, write, total)
  )

  implicit val cogStepTime: Cogen[StepTime] =
    Cogen[(TimeSpan, TimeSpan, TimeSpan, TimeSpan, TimeSpan)].contramap(t =>
      (t.configChange, t.exposure, t.readout, t.write, t.total)
    )
}

object ArbStepTime extends ArbStepTime
