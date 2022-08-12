// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class StepStage(val tag: String) extends Product with Serializable

object StepStage {
  /** @group Constructors */ case object EndConfigure extends StepStage("end_configure")
  /** @group Constructors */ case object EndObserve extends StepStage("end_observe")
  /** @group Constructors */ case object EndStep extends StepStage("end_step")
  /** @group Constructors */ case object StartConfigure extends StepStage("start_configure")
  /** @group Constructors */ case object StartObserve extends StepStage("start_observe")
  /** @group Constructors */ case object StartStep extends StepStage("start_step")

  implicit val StepStageEnumerated: Enumerated[StepStage] =
    Enumerated.from(
      EndConfigure,
      EndObserve,
      EndStep,
      StartConfigure,
      StartObserve,
      StartStep
    ).withTag(_.tag)
}
