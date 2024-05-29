// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class Breakpoint(val tag: String) extends Product with Serializable

object Breakpoint {
  /** @group Constructors */ case object Enabled extends Breakpoint("enabled")
  /** @group Constructors */ case object Disabled extends Breakpoint("disabled")

  given Enumerated[Breakpoint] =
    Enumerated.from(Enabled, Disabled).withTag(_.tag)

}
