// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed trait StepStage extends Product with Serializable

object StepStage {
  /** @group Constructors */ case object EndConfigure extends StepStage
  /** @group Constructors */ case object EndObserve extends StepStage
  /** @group Constructors */ case object EndStep extends StepStage
  /** @group Constructors */ case object StartConfigure extends StepStage
  /** @group Constructors */ case object StartObserve extends StepStage
  /** @group Constructors */ case object StartStep extends StepStage

  implicit val StepStageEnumerated: Enumerated[StepStage] =
    Enumerated.of(
      EndConfigure,
      EndObserve,
      EndStep,
      StartConfigure,
      StartObserve,
      StartStep
    )
}
