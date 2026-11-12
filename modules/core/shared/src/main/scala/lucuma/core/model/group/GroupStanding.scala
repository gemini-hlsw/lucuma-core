// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import lucuma.core.enums.GroupStandingStatus
import lucuma.core.model.Group

/**
 * Every group and observation has a group standing.  It says whether the group
 * or observation is still worth doing, given the groups it belongs to.  For
 * example, a `Ready` observation in a group that can no longer be completed
 * is `Unschedulable`: completing it couldn't help complete the group.
 * Similarly, the observations left over in a group that already has the
 * observations it requires are `Complete`: they're no longer needed.
 *
 * The standing comes from the progress (`GroupProgress`) of the groups
 * containing the observation, or, for a group, the groups containing it and
 * the group itself.  The outermost of these that is complete or unschedulable
 * decides the standing and is named in it.  If none is, the standing is
 * `Viable`.  An observation outside any group is always `Viable`.
 *
 * Together with the observation workflow state, the standing can filter the
 * observations to consider for scheduling: only `Viable` ones need further
 * consideration.  The ODB calculates the standing in the background, so it may
 * be out of date while a calculation is pending.
 */
sealed trait GroupStanding:
  def status: GroupStandingStatus

object GroupStanding:

  /** None of the groups it belongs to is complete or unschedulable. */
  case object Viable extends GroupStanding:
    override def status: GroupStandingStatus = GroupStandingStatus.Viable

  /** `groupId` is complete, so it is no longer needed. */
  final case class Complete(
    groupId: Group.Id
  ) extends GroupStanding:
    override def status: GroupStandingStatus = GroupStandingStatus.Complete

  /** `groupId` can no longer be completed, so doing it couldn't help. */
  final case class Unschedulable(
    groupId: Group.Id
  ) extends GroupStanding:
    override def status: GroupStandingStatus = GroupStandingStatus.Unschedulable

  given Eq[GroupStanding] = Eq.fromUniversalEquals
