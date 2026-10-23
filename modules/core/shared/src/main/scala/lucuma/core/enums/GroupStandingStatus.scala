// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * The status of a `GroupStanding`: whether a group or observation is still
 * worth doing, given the groups it belongs to.  See `GroupStanding` for how
 * it's decided.
 */
enum GroupStandingStatus(val tag: String) derives Enumerated:

  /** None of the groups it belongs to is complete or unschedulable. */
  case Viable        extends GroupStandingStatus("viable")

  /** A group it belongs to is complete, so it is no longer needed. */
  case Complete      extends GroupStandingStatus("complete")

  /**
   * A group it belongs to can no longer be completed without human
   * intervention, so doing it couldn't help.
   */
  case Unschedulable extends GroupStandingStatus("unschedulable")

object GroupStandingStatus:

  given Display[GroupStandingStatus] =
    Display.byShortName(_.tag.capitalize)
