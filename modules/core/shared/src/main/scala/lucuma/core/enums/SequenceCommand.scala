// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class SequenceCommand(val tag: String) extends Product with Serializable

object SequenceCommand {
  /** @group Constructors */ case object Abort extends SequenceCommand("abort")
  /** @group Constructors */ case object Continue extends SequenceCommand("continue")
  /** @group Constructors */ case object Pause extends SequenceCommand("pause")
  /** @group Constructors */ case object Slew extends SequenceCommand("slew")
  /** @group Constructors */ case object Start extends SequenceCommand("start")
  /** @group Constructors */ case object Stop extends SequenceCommand("stop")

  implicit val SequenceCommandEnumerated: Enumerated[SequenceCommand] =
    Enumerated.from(
      Abort,
      Continue,
      Pause,
      Slew,
      Start,
      Stop
    ).withTag(_.tag)
}
