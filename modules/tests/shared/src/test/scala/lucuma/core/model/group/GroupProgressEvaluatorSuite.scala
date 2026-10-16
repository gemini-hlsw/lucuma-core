// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.GroupConstraintViolationType
import lucuma.core.enums.GroupConstraintViolationType.*
import lucuma.core.enums.GroupStatus
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import munit.FunSuite

import java.time.Instant

class GroupProgressEvaluatorSuite extends FunSuite:

  import GroupProgressNode.*

  def oid(n: Long): Observation.Id = Observation.Id.fromLong(n).get
  def gid(n: Long): Group.Id       = Group.Id.fromLong(n).get

  def ts(s: String): Timestamp     = Timestamp.unsafeFromInstant(Instant.parse(s))
  def hours(h: Int): TimeSpan      = TimeSpan.fromHours(h).get
  def minutes(m: Int): TimeSpan    = TimeSpan.fromMinutes(m).get

  // The night of 15-16 October 2026 at both sites.
  val t0: Timestamp = ts("2026-10-16T05:00:00Z") // 19:00 HST, 02:00 CLT

  def leaf(
    n:     Long,
    state: ExecutionState    = ExecutionState.NotStarted,
    time:  Option[Timestamp] = None,
    site:  Site              = Site.GN,
    inactive: Boolean        = false
  ): Leaf =
    Leaf(oid(n), state, inactive, time, Some(site))

  def done(n: Long, t: Timestamp, site: Site = Site.GN): Leaf =
    leaf(n, ExecutionState.Completed, Some(t), site)

  def branch(
    n:         Long,
    children:  List[GroupProgressNode],
    min:       Option[Int]      = None,
    ordered:   Boolean          = false,
    minI:      Option[TimeSpan] = None,
    maxI:      Option[TimeSpan] = None,
    sameNight: Boolean          = false
  ): Branch =
    Branch(gid(n), min.map(m => NonNegShort.unsafeFrom(m.toShort)), ordered, minI, maxI, sameNight, children)

  def eval(b: Branch, now: Timestamp = t0): GroupProgress =
    GroupProgressEvaluator.evaluate(b, now)(b.groupId)

  def progress(
    status:     GroupStatus,
    completed:  Int,
    required:   Int,
    nextWindow: Option[GroupWindow]            = None,
    next:       List[Observation.Id]           = Nil,
    violations: List[GroupConstraintViolation] = Nil
  ): GroupProgress =
    GroupProgress(status, NonNegInt.unsafeFrom(completed), NonNegInt.unsafeFrom(required), nextWindow, next, violations)

  def violation(
    t:    GroupConstraintViolationType,
    g:    Long,
    obs:  List[Long] = Nil,
    subs: List[Long] = Nil
  ): GroupConstraintViolation =
    GroupConstraintViolation(t, gid(g), obs.map(oid), subs.map(gid))

  // Counting

  test("nothing started: pending"):
    assertEquals(eval(branch(1, List(leaf(1), leaf(2)))), progress(GroupStatus.Pending, 0, 2))

  test("one element executing: ongoing"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.Ongoing), leaf(2)))),
      progress(GroupStatus.Ongoing, 0, 2)
    )

  test("declared ongoing counts as started, not complete"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.DeclaredOngoing)))),
      progress(GroupStatus.Ongoing, 0, 1)
    )

  test("n of m met: complete"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2), done(3, t0 +| hours(1))), min = Some(2))),
      progress(GroupStatus.Complete, 2, 2)
    )

  test("complete while another element is still executing"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, ExecutionState.Ongoing)), min = Some(1))).status,
      GroupStatus.Complete
    )

  test("declared complete counts as complete"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.DeclaredComplete)))).status,
      GroupStatus.Complete
    )

  test("inactive observations don't count toward 'all required'"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, inactive = true), done(3, t0)))),
      progress(GroupStatus.Complete, 2, 2)
    )

  test("an inactive observation that is complete still counts"):
    assertEquals(
      eval(branch(1, List(leaf(1, ExecutionState.Completed, Some(t0), inactive = true), leaf(2)))),
      progress(GroupStatus.Ongoing, 1, 2)
    )

  test("too few viable elements: insufficient elements"):
    assertEquals(
      eval(branch(1, List(leaf(1), leaf(2, inactive = true), leaf(3)), min = Some(3))),
      progress(GroupStatus.Unschedulable, 0, 3, violations = List(violation(InsufficientElements, 1)))
    )

  test("a group with nothing viable is pending, and its parent leaves it out"):
    val empty  = branch(2, List(leaf(2, inactive = true), leaf(3, inactive = true)))
    val parent = branch(1, List(done(1, t0), empty))
    val all    = GroupProgressEvaluator.evaluate(parent, t0)
    assertEquals(all(gid(2)), progress(GroupStatus.Pending, 0, 0))
    assertEquals(all(gid(1)), progress(GroupStatus.Complete, 1, 1))

  // Order

  test("ordered: the next observation is the first incomplete one"):
    assertEquals(
      eval(branch(1, List(leaf(1), leaf(2)), ordered = true)),
      progress(GroupStatus.Pending, 0, 2, next = List(oid(1)))
    )
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), ordered = true)),
      progress(GroupStatus.Ongoing, 1, 2, next = List(oid(2)))
    )

  test("ordered: a later element completed before an earlier incomplete one"):
    assertEquals(
      eval(branch(1, List(leaf(1), done(2, t0)), ordered = true)),
      progress(GroupStatus.Unschedulable, 1, 2, violations = List(violation(Order, 1, List(1, 2))))
    )

  test("ordered: completion times out of order"):
    assertEquals(
      eval(branch(1, List(done(1, t0 +| hours(1)), done(2, t0)), ordered = true)).violations,
      List(violation(Order, 1, List(1, 2)))
    )

  test("ordered n of m: requires the first n"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(1)), leaf(3)), min = Some(2), ordered = true)),
      progress(GroupStatus.Complete, 2, 2)
    )
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2), done(3, t0 +| hours(1))), min = Some(2), ordered = true)).violations,
      List(violation(Order, 1, List(2, 3)))
    )

  // Intervals

  test("completions closer than the minimum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| minutes(30))), minI = Some(hours(1)))).violations,
      List(violation(MinimumInterval, 1, List(1, 2)))
    )

  test("completions further apart than the maximum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(2))), maxI = Some(hours(1)))).violations,
      List(violation(MaximumInterval, 1, List(1, 2)))
    )

  test("intervals compare consecutive completions in completion order"):
    val g = branch(1, List(done(1, t0 +| hours(4)), done(2, t0), done(3, t0 +| hours(2))), minI = Some(hours(1)), maxI = Some(hours(3)))
    assertEquals(eval(g), progress(GroupStatus.Complete, 3, 3))

  test("maximum interval deadline missed: unschedulable"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), maxI = Some(hours(1))), now = t0 +| hours(2)),
      progress(GroupStatus.Unschedulable, 1, 2, violations = List(violation(MaximumInterval, 1, List(1))))
    )

  test("finishing after the maximum interval is a violation even if started within it"):
    assertEquals(
      eval(branch(1, List(done(1, t0), done(2, t0 +| hours(2))), maxI = Some(hours(1)))).status,
      GroupStatus.Unschedulable
    )

  test("next window: from the minimum to the maximum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), minI = Some(hours(1)), maxI = Some(hours(3))), now = t0 +| minutes(30)),
      progress(GroupStatus.Ongoing, 1, 2, nextWindow = Some(GroupWindow(t0 +| hours(1), Some(t0 +| hours(3)))))
    )

  test("next window: open-ended with only a minimum interval"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2)), minI = Some(hours(1)))).nextWindow,
      Some(GroupWindow(t0 +| hours(1), None))
    )

  test("no window without interval or night constraints"):
    assertEquals(eval(branch(1, List(done(1, t0), leaf(2)))).nextWindow, None)

  test("zero intervals are unset"):
    val g = branch(1, List(done(1, t0), done(2, t0 +| hours(48)), leaf(3)), minI = Some(TimeSpan.Zero), maxI = Some(TimeSpan.Zero))
    assertEquals(eval(g, now = t0 +| hours(100)), progress(GroupStatus.Ongoing, 2, 3))

  test("declared complete without a time is counted but not interval-checked"):
    val g = branch(1, List(done(1, t0), leaf(2, ExecutionState.DeclaredComplete)), maxI = Some(hours(1)))
    assertEquals(eval(g, now = t0 +| hours(5)), progress(GroupStatus.Complete, 2, 2))

  // Same night

  test("same night: completions on the same calendar night at different sites"):
    val g = branch(1, List(done(1, t0, Site.GN), done(2, t0 +| hours(1), Site.GS)), sameNight = true)
    assertEquals(eval(g), progress(GroupStatus.Complete, 2, 2))

  test("same night: completions on different nights"):
    // 17:00 CLT on 16 October is the start of the night of 16-17 October.
    val g = branch(1, List(done(1, t0, Site.GN), done(2, ts("2026-10-16T20:00:00Z"), Site.GS)), sameNight = true)
    assertEquals(eval(g).violations, List(violation(SameNight, 1, List(1, 2))))

  test("same night: window ends with the night at the remaining site"):
    // The GN night of 15-16 October ends at 14:00 HST, 00:00 UT on the 17th.
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, site = Site.GN)), sameNight = true), now = t0 +| hours(1)).nextWindow,
      Some(GroupWindow(t0, Some(ts("2026-10-17T00:00:00Z"))))
    )

  test("same night: the night ended with an element still to observe"):
    assertEquals(
      eval(branch(1, List(done(1, t0), leaf(2, site = Site.GN)), sameNight = true), now = ts("2026-10-17T01:00:00Z")),
      progress(GroupStatus.Unschedulable, 1, 2, violations = List(violation(SameNight, 1, List(1))))
    )

  // Subgroups

  test("a needed subgroup that is unschedulable sinks its parent"):
    val sub    = branch(2, List(leaf(2), leaf(3, inactive = true)), min = Some(2))
    val parent = branch(1, List(leaf(1), sub))
    assertEquals(
      eval(parent).violations,
      List(violation(SubgroupUnschedulable, 1, subs = List(2)))
    )

  test("an unschedulable subgroup the parent doesn't need is harmless"):
    val sub    = branch(2, List(leaf(2), leaf(3, inactive = true)), min = Some(2))
    val parent = branch(1, List(done(1, t0), sub, leaf(4)), min = Some(2))
    assertEquals(eval(parent), progress(GroupStatus.Ongoing, 1, 2))

  test("a complete subgroup counts as one element, completed at its deciding completion"):
    val sub    = branch(2, List(done(2, t0 +| hours(1)), done(3, t0 +| hours(3)), leaf(4)), min = Some(2))
    // The subgroup completed at t0 + 3h, more than an hour after observation 1.
    val parent = branch(1, List(done(1, t0), sub), maxI = Some(hours(1)))
    val all    = GroupProgressEvaluator.evaluate(parent, t0 +| hours(4))
    assertEquals(all(gid(2)).status, GroupStatus.Complete)
    assertEquals(all(gid(1)).violations, List(violation(MaximumInterval, 1, List(1), List(2))))

  test("ordered parent: the next element is a subgroup, so name its remaining observations"):
    val sub    = branch(2, List(done(2, t0), leaf(3), leaf(4)))
    val parent = branch(1, List(done(1, t0), sub, leaf(5)), ordered = true)
    assertEquals(eval(parent).nextObservationIds, List(oid(3), oid(4)))

  test("every group in the tree is evaluated"):
    val parent = branch(1, List(branch(2, List(leaf(1))), branch(3, List(branch(4, List(leaf(2)))))))
    assertEquals(GroupProgressEvaluator.evaluate(parent, t0).keySet, Set(1L, 2L, 3L, 4L).map(gid))
