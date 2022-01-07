// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.`enum`

import lucuma.core.util.Enumerated

sealed trait SequenceCommand extends Product with Serializable

object SequenceCommand {
  /** @group Constructors */ case object Abort extends SequenceCommand
  /** @group Constructors */ case object Continue extends SequenceCommand
  /** @group Constructors */ case object Pause extends SequenceCommand
  /** @group Constructors */ case object Slew extends SequenceCommand
  /** @group Constructors */ case object Start extends SequenceCommand
  /** @group Constructors */ case object Stop extends SequenceCommand

  implicit val SequenceCommandEnumerated: Enumerated[SequenceCommand] =
    Enumerated.of(
      Abort,
      Continue,
      Pause,
      Slew,
      Start,
      Stop
    )
}
