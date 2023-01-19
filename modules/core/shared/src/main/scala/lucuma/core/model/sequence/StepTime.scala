// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time._

final case class StepTime(
  configChange: TimeSpan,
  exposure:     TimeSpan,
  readout:      TimeSpan,
  write:        TimeSpan,
  total:        TimeSpan
)

object StepTime {
  implicit val eqStepTime: Eq[StepTime] =
    Eq.by(x => (x.configChange, x.exposure, x.readout, x.write, x.total))

  /** @group Optics */
  val configChange: Lens[StepTime, TimeSpan] =
    Focus[StepTime](_.configChange)

  /** @group Optics */
  val exposure: Lens[StepTime, TimeSpan] =
    Focus[StepTime](_.exposure)

  /** @group Optics */
  val readout: Lens[StepTime, TimeSpan] =
    Focus[StepTime](_.readout)

  /** @group Optics */
  val write: Lens[StepTime, TimeSpan] =
    Focus[StepTime](_.write)

  /** @group Optics */
  val total: Lens[StepTime, TimeSpan] =
    Focus[StepTime](_.total)
}
