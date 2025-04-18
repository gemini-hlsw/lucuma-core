// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
  * Enumerated type for observe lucuma.core.Event Event types.
  * @group Enumerations
  */
enum EventType(val tag: String) derives Enumerated:
  /** @group Constructors */ case StartSequence extends EventType("StartSequence")
  /** @group Constructors */ case EndSequence extends EventType("EndSequence")
  /** @group Constructors */ case StartSlew extends EventType("StartSlew")
  /** @group Constructors */ case EndSlew extends EventType("EndSlew")
  /** @group Constructors */ case StartVisit extends EventType("StartVisit")
  /** @group Constructors */ case EndVisit extends EventType("EndVisit")
  /** @group Constructors */ case StartIntegration extends EventType("StartIntegration")
  /** @group Constructors */ case EndIntegration extends EventType("EndIntegration")
  /** @group Constructors */ case Abort extends EventType("Abort")
  /** @group Constructors */ case Continue extends EventType("Continue")
  /** @group Constructors */ case Pause extends EventType("Pause")
  /** @group Constructors */ case Stop extends EventType("Stop")

