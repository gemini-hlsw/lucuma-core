// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Enumerated

sealed trait Breakpoint extends Product with Serializable

object Breakpoint {
  /** @group Constructors */ case object Enabled extends Breakpoint
  /** @group Constructors */ case object Disabled extends Breakpoint

  implicit val BreakpointEnumerated: Enumerated[Breakpoint] = Enumerated.of(Enabled, Disabled)

}