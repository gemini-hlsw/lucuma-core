// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class DatasetStage(val tag: String) extends Product with Serializable

object DatasetStage {
  /** @group Constructors */ case object EndObserve extends DatasetStage("end_observe")
  /** @group Constructors */ case object EndReadout extends DatasetStage("end_readout")
  /** @group Constructors */ case object EndWrite extends DatasetStage("end_write")
  /** @group Constructors */ case object StartObserve extends DatasetStage("start_observe")
  /** @group Constructors */ case object StartReadout extends DatasetStage("start_readout")
  /** @group Constructors */ case object StartWrite extends DatasetStage("start_write")

  implicit val DataStageEnumerated: Enumerated[DatasetStage] =
    Enumerated.from(
      EndObserve,
      EndReadout,
      EndWrite,
      StartObserve,
      StartReadout,
      StartWrite
    ).withTag(_.tag)

}
