// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * A reason a group can no longer be completed: a constraint it can no longer
 * meet.
 */
enum GroupConstraintViolation(val tag: String) derives Enumerated:

  /**
   * In an ordered group, an element completed before an earlier one.
   */
  case Order                 extends GroupConstraintViolation("order")

  /**
   * An element started before the minimum interval had passed since the
   * previous completion.
   */
  case MinimumInterval       extends GroupConstraintViolation("minimum_interval")

  /**
   * An element completed after the maximum interval had passed since the
   * previous completion, or the maximum interval has passed since the last
   * completion without another.
   */
  case MaximumInterval       extends GroupConstraintViolation("maximum_interval")

  /**
   * In a same-night group, completions fell on different nights, or the night
   * of the completions ends (or ends before the minimum interval passes) with
   * elements still to observe.
   */
  case SameNight             extends GroupConstraintViolation("same_night")

  /**
   * A same-night group has observations at more than one site, which it
   * cannot support.
   */
  case SameNightMultipleSites extends GroupConstraintViolation("same_night_multiple_sites")

  /**
   * A night on which at least one counted observation completed ended with
   * fewer than the group's minimum observations per night.
   */
  case ObservationsPerNightMinimum extends GroupConstraintViolation("observations_per_night_minimum")

  /**
   * More than the group's maximum observations per night completed on a
   * single night.
   */
  case ObservationsPerNightMaximum extends GroupConstraintViolation("observations_per_night_maximum")

  /**
   * A group with observations-per-night limits has observations at more than
   * one site, so its nights are ambiguous.
   */
  case ObservationsPerNightMultipleSites extends GroupConstraintViolation("observations_per_night_multiple_sites")

  /**
   * Too few viable (not inactive) elements remain to reach the minimum.
   */
  case InsufficientElements  extends GroupConstraintViolation("insufficient_elements")

  /**
   * A subgroup that the group needs can no longer be completed.
   */
  case SubgroupUnschedulable extends GroupConstraintViolation("subgroup_unschedulable")
