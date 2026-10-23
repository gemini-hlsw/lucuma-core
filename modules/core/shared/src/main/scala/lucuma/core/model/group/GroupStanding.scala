// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import lucuma.core.enums.GroupStandingStatus
import lucuma.core.model.Group

/**
 * Whether the groups a group or observation belongs to still want work in it.
 * For a group these are the group itself and its ancestors; an observation
 * stands as its group does, and one outside any group is `Viable`.
 *
 * Their progress decides it: when one of them is complete or can no longer be
 * completed, the outermost such group decides, and is named.  This can differ
 * from a group's own progress.  A group that is progressing normally inside a
 * parent that can no longer be completed is itself `Unschedulable`.
 */
sealed trait GroupStanding:
  def status: GroupStandingStatus

object GroupStanding:

  /** None of those groups is complete or unschedulable. */
  case object Viable extends GroupStanding:
    override def status: GroupStandingStatus = GroupStandingStatus.Viable

  /** `groupId` is complete, so work here is no longer needed. */
  final case class Complete(
    groupId: Group.Id
  ) extends GroupStanding:
    override def status: GroupStandingStatus = GroupStandingStatus.Complete

  /** `groupId` can no longer be completed, so work here can no longer help. */
  final case class Unschedulable(
    groupId: Group.Id
  ) extends GroupStanding:
    override def status: GroupStandingStatus = GroupStandingStatus.Unschedulable

  given Eq[GroupStanding] = Eq.fromUniversalEquals
