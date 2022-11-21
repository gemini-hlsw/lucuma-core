// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time._

import java.time.Duration

final case class StepTime(
  configChange: Duration,
  exposure:     Duration,
  readout:      Duration,
  write:        Duration,
  total:        Duration
)

object StepTime {
  implicit val eqStepTime: Eq[StepTime] =
    Eq.by(x => (x.configChange, x.exposure, x.readout, x.write, x.total))

  /** @group Optics */
  val configChange: Lens[StepTime, Duration] =
    Focus[StepTime](_.configChange)

  /** @group Optics */
  val exposure: Lens[StepTime, Duration] =
    Focus[StepTime](_.exposure)

  /** @group Optics */
  val readout: Lens[StepTime, Duration] =
    Focus[StepTime](_.readout)

  /** @group Optics */
  val write: Lens[StepTime, Duration] =
    Focus[StepTime](_.write)

  /** @group Optics */
  val total: Lens[StepTime, Duration] =
    Focus[StepTime](_.total)
}
