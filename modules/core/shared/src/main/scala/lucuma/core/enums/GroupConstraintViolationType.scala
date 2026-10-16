// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * A reason a group can no longer be completed.
 */
enum GroupConstraintViolationType(val tag: String) derives Enumerated:

  /** In an ordered group, an element completed before an earlier one. */
  case Order                 extends GroupConstraintViolationType("order")

  /** Two consecutive completions were closer than the minimum interval. */
  case MinimumInterval       extends GroupConstraintViolationType("minimum_interval")

  /**
   * Two consecutive completions were further apart than the maximum interval,
   * or the maximum interval has passed since the last completion without
   * another.
   */
  case MaximumInterval       extends GroupConstraintViolationType("maximum_interval")

  /**
   * In a same-night group, completions fell on different nights, or the night
   * of the completions has ended at every site still to observe.
   */
  case SameNight             extends GroupConstraintViolationType("same_night")

  /** Too few viable (not inactive) elements remain to reach the minimum. */
  case InsufficientElements  extends GroupConstraintViolationType("insufficient_elements")

  /**
   * A subgroup that the group needs can no longer be completed.
   */
  case SubgroupUnschedulable extends GroupConstraintViolationType("subgroup_unschedulable")
