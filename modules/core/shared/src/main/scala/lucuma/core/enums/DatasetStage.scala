// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed trait DatasetStage extends Product with Serializable

object DatasetStage {
  /** @group Constructors */ case object EndObserve extends DatasetStage
  /** @group Constructors */ case object EndReadout extends DatasetStage
  /** @group Constructors */ case object EndWrite extends DatasetStage
  /** @group Constructors */ case object StartObserve extends DatasetStage
  /** @group Constructors */ case object StartReadout extends DatasetStage
  /** @group Constructors */ case object StartWrite extends DatasetStage

  implicit val DataStageEnumerated: Enumerated[DatasetStage] =
    Enumerated.of(
      EndObserve,
      EndReadout,
      EndWrite,
      StartObserve,
      StartReadout,
      StartWrite
    )

}
