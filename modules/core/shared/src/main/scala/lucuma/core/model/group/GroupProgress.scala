// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import cats.data.NonEmptySet
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GroupConstraintViolation
import lucuma.core.enums.GroupStatus
import lucuma.core.util.TimestampInterval

/**
 * A group's progress toward satisfying its constraints, with the information
 * relevant to each status.
 *
 * Where they apply, `completed` counts the group's complete elements that
 * count toward it (a subgroup counts as one element) and `required` is how
 * many it needs.  An ordered group's elements after the first `required`
 * don't count, nor do an unordered group's completions after it is complete.
 */
sealed trait GroupProgress:
  def status: GroupStatus

object GroupProgress:

  /**
   * The group has nothing to observe: no elements, or only inactive
   * observations and empty subgroups.  It requires nothing, and its parent
   * group leaves it out.
   */
  case object Empty extends GroupProgress:
    override def status: GroupStatus = GroupStatus.Empty

  /** Nothing has started, so nothing is complete. */
  final case class Pending(
    required: PosInt
  ) extends GroupProgress:
    override def status: GroupStatus = GroupStatus.Pending

  /**
   * Execution has started, and fewer elements are complete than the group
   * requires (with enough, it would be complete or unschedulable).
   *
   * `nextWindow` is set when the group's constraints restrict when the next
   * element may be observed, given the completions so far.  Its `start` is
   * the earliest time the element may *start*, per the minimum interval.  Its
   * `end` is the latest time it may *finish*, per the maximum interval or the
   * end of the night for a same-night group, or `Timestamp.Max` when there is
   * no such limit.  Fitting the observation between the two (say, by its time
   * estimate) is left to the scheduler.
   *
   * The window reflects only the group's own constraints.  The program's
   * active period and each observation's timing windows also apply, and are
   * left to the scheduler to combine with it.
   */
  final case class Ongoing(
    completed:  NonNegInt,
    required:   PosInt,
    nextWindow: Option[TimestampInterval]
  ) extends GroupProgress:
    override def status: GroupStatus = GroupStatus.Ongoing

  /**
   * At least as many elements are complete as the group requires, and every
   * constraint is met.
   */
  final case class Complete(
    completed: NonNegInt,
    required:  PosInt
  ) extends GroupProgress:
    override def status: GroupStatus = GroupStatus.Complete

  /**
   * The group can no longer be completed, for the given reasons: the
   * constraints it can no longer meet.
   */
  final case class Unschedulable(
    completed:  NonNegInt,
    required:   PosInt,
    violations: NonEmptySet[GroupConstraintViolation]
  ) extends GroupProgress:
    override def status: GroupStatus = GroupStatus.Unschedulable

  given Eq[GroupProgress] = Eq.fromUniversalEquals
