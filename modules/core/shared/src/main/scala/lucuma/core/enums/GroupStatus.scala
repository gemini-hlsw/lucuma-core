// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Where a group stands in satisfying its constraints: whether it has anything
 * to observe, has started, is complete, or can no longer be completed.
 */
enum GroupStatus(val tag: String) derives Enumerated:

  /**
   * The group has nothing to observe: no elements, or only inactive
   * observations and empty subgroups.  Its parent group leaves it out.
   */
  case Empty         extends GroupStatus("empty")

  /**
   * Nothing in the group has started executing (slews alone don't count), and
   * the group can still be completed.
   */
  case Pending       extends GroupStatus("pending")

  /** Execution has started and the group can still be completed. */
  case Ongoing       extends GroupStatus("ongoing")

  /**
   * Enough elements are complete and every constraint is met.  An element may
   * still be executing.
   */
  case Complete      extends GroupStatus("complete")

  /** The group can no longer be completed without human intervention. */
  case Unschedulable extends GroupStatus("unschedulable")
