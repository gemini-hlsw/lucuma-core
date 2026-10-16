// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.GroupConstraintViolationType
import lucuma.core.enums.GroupStatus
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.util.Timestamp

/**
 * When the next element of a group may be observed, given the completions so
 * far.  The start is the earliest time the minimum interval allows, and the
 * end (exclusive), if any, is the latest time the maximum interval or the
 * same-night requirement allows.
 */
final case class GroupWindow(
  start: Timestamp,
  end:   Option[Timestamp]
) derives Eq

/**
 * A reason the group `groupId` can no longer be completed, with the elements
 * involved: observations directly in the group and subgroups of it.
 */
final case class GroupConstraintViolation(
  violationType:  GroupConstraintViolationType,
  groupId:        Group.Id,
  observationIds: List[Observation.Id],
  subgroupIds:    List[Group.Id]
) derives Eq

/**
 * A group's progress toward satisfying its constraints.
 *
 * `completed` counts the group's complete elements (a subgroup counts as one
 * element) and `required` is how many it needs.  `nextWindow` is set while the
 * group is ongoing and its constraints restrict when the next element may be
 * observed.  For an ordered group, `nextObservationIds` names the observations
 * that may be observed next.  `violations` is empty unless the group is
 * unschedulable.
 */
final case class GroupProgress(
  status:             GroupStatus,
  completed:          NonNegInt,
  required:           NonNegInt,
  nextWindow:         Option[GroupWindow],
  nextObservationIds: List[Observation.Id],
  violations:         List[GroupConstraintViolation]
) derives Eq
