// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Progress of a group toward satisfying its constraints: the number of
 * elements it requires, their order, the intervals between them and whether
 * they must share a night.
 */
enum GroupStatus(val tag: String) derives Enumerated:

  /**
   * Nothing in the group has started executing (slews alone don't count), and
   * the group can still be completed.  A group with no viable elements at all
   * is also pending.
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
