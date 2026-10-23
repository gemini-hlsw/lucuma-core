// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Whether the groups an observation or group belongs to still want work in
 * it.  For a group these are the group itself and the groups containing it;
 * an observation stands as its group does.
 */
enum GroupStandingStatus(val tag: String) derives Enumerated:

  /** None of those groups is complete or unschedulable. */
  case Viable        extends GroupStandingStatus("viable")

  /** One of those groups is complete, so the work is no longer needed. */
  case Complete      extends GroupStandingStatus("complete")

  /**
   * One of those groups can no longer be completed, so the work can no longer
   * help without human intervention.
   */
  case Unschedulable extends GroupStandingStatus("unschedulable")
