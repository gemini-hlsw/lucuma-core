// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import cats.derived.*
import lucuma.core.enums.ComaOption
import lucuma.core.enums.TipTiltSource

/** Data type for M2 guide config. */
sealed trait M2GuideConfig extends Product with Serializable derives Eq {
  def uses(s: TipTiltSource): Boolean
}

object M2GuideConfig {
  case object M2GuideOff extends M2GuideConfig derives Eq, Show {
    override def uses(s: TipTiltSource): Boolean = false
  }

  case class M2GuideOn(coma: ComaOption, sources: Set[TipTiltSource]) extends M2GuideConfig derives Eq {
    override def uses(s: TipTiltSource): Boolean = sources.contains(s)
  }
}
