// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.util.Interval
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time._

final case class StepTime(
  configChange: Interval,
  exposure:     Interval,
  readout:      Interval,
  write:        Interval,
  total:        Interval
)

object StepTime {
  implicit val eqStepTime: Eq[StepTime] =
    Eq.by(x => (x.configChange, x.exposure, x.readout, x.write, x.total))

  /** @group Optics */
  val configChange: Lens[StepTime, Interval] =
    Focus[StepTime](_.configChange)

  /** @group Optics */
  val exposure: Lens[StepTime, Interval] =
    Focus[StepTime](_.exposure)

  /** @group Optics */
  val readout: Lens[StepTime, Interval] =
    Focus[StepTime](_.readout)

  /** @group Optics */
  val write: Lens[StepTime, Interval] =
    Focus[StepTime](_.write)

  /** @group Optics */
  val total: Lens[StepTime, Interval] =
    Focus[StepTime](_.total)
}
