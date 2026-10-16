// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.syntax.order.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.GroupConstraintViolationType
import lucuma.core.enums.GroupStatus
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

/**
 * Evaluates the progress of groups toward satisfying their constraints.
 *
 * The rules:
 *
 *   - An inactive observation that isn't complete is left out of its group
 *     entirely, as is a subgroup left with no elements.  A group with no
 *     elements is pending.
 *   - A group requires `minimumRequired` elements, or all of them when unset.
 *     An ordered group requires the first ones, in order.  A subgroup counts
 *     as one element, complete when its own status is complete.
 *   - Zero intervals are treated as unset.  The intervals are checked between
 *     consecutive completions, in completion order.  Completions with no known
 *     time are counted but not checked.
 *   - A same-night group's completions must fall on the same local observing
 *     night (the same calendar night at every site).
 *   - Any violation makes the group unschedulable, including a deadline that
 *     `now` has passed: the maximum interval since the last completion, or the
 *     end of the completions' night at every site still to observe.
 */
object GroupProgressEvaluator:

  /** Evaluates every group in the tree. */
  def evaluate(tree: GroupProgressNode, now: Timestamp): Map[Group.Id, GroupProgress] =
    tree match
      case _: GroupProgressNode.Leaf   => Map.empty
      case b: GroupProgressNode.Branch => evaluateBranch(b, now)._2

  /** Evaluates every group in each of the trees. */
  def evaluate(trees: List[GroupProgressNode], now: Timestamp): Map[Group.Id, GroupProgress] =
    trees.foldLeft(Map.empty[Group.Id, GroupProgress])((m, t) => m ++ evaluate(t, now))

  // A group element as its parent sees it.
  private final case class Element(
    observationIds: List[Observation.Id],
    subgroupIds:    List[Group.Id],
    excluded:       Boolean,                  // left out of the group entirely
    complete:       Boolean,
    unschedulable:  Boolean,
    started:        Boolean,
    completionTime: Option[Timestamp],
    nights:         Set[LocalObservingNight], // nights of the completions it counts
    remainingSites: Set[Site],                // sites of observations still to do
    candidates:     List[Observation.Id]      // observations that may be done next
  ):
    def isRemaining: Boolean =
      !complete && !unschedulable

  private given Ordering[Timestamp] = cats.Order[Timestamp].toOrdering

  private def leafElement(leaf: GroupProgressNode.Leaf): Element =
    import ExecutionState.*
    val complete  = leaf.executionState match
      case Completed | DeclaredComplete => true
      case _                            => false
    val started   = leaf.executionState match
      case NotDefined | NotStarted => false
      case _                       => true
    val excluded  = leaf.inactive && !complete
    val remaining = !complete && !excluded
    val time      = Option.when(complete)(leaf.completionTime).flatten

    Element(
      observationIds = List(leaf.observationId),
      subgroupIds    = Nil,
      excluded       = excluded,
      complete       = complete,
      unschedulable  = false,
      started        = started,
      completionTime = time,
      nights         = (for t <- time; s <- leaf.site yield night(s, t)).toSet,
      remainingSites = if remaining then leaf.site.toSet else Set.empty,
      candidates     = if remaining then List(leaf.observationId) else Nil
    )

  private def night(site: Site, time: Timestamp): LocalObservingNight =
    LocalObservingNight.fromSiteAndInstant(site, time.toInstant)

  // The end of `n` at the given sites, or at every site if none are known.
  private def nightEnds(n: LocalObservingNight, sites: Set[Site]): List[Timestamp] =
    (if sites.isEmpty then Site.values.toList else sites.toList).map: s =>
      Timestamp.fromInstantTruncatedAndBounded(n.atSite(s).end)

  private def evaluateBranch(
    branch: GroupProgressNode.Branch,
    now:    Timestamp
  ): (Element, Map[Group.Id, GroupProgress]) =

    val evaluated: List[(Element, Map[Group.Id, GroupProgress])] =
      branch.children.map:
        case l: GroupProgressNode.Leaf   => (leafElement(l), Map.empty)
        case b: GroupProgressNode.Branch => evaluateBranch(b, now)

    val nested: Map[Group.Id, GroupProgress] =
      evaluated.foldLeft(Map.empty[Group.Id, GroupProgress])(_ ++ _._2)

    val elements: List[Element] =
      evaluated.map(_._1).filterNot(_.excluded)

    val (progress, element) =
      if elements.isEmpty then emptyGroup(branch.groupId)
      else evaluateElements(branch, elements, now)

    (element, nested + (branch.groupId -> progress))

  private def emptyGroup(gid: Group.Id): (GroupProgress, Element) =
    (
      GroupProgress(GroupStatus.Pending, NonNegInt.unsafeFrom(0), NonNegInt.unsafeFrom(0), None, Nil, Nil),
      Element(Nil, List(gid), excluded = true, complete = false, unschedulable = false, started = false, None, Set.empty, Set.empty, Nil)
    )

  private def evaluateElements(
    branch:   GroupProgressNode.Branch,
    elements: List[Element],
    now:      Timestamp
  ): (GroupProgress, Element) =

    val gid         = branch.groupId
    val minInterval = branch.minimumInterval.filterNot(_.isZero)
    val maxInterval = branch.maximumInterval.filterNot(_.isZero)
    val required    = branch.minimumRequired.fold(elements.size)(_.value.toInt)
    val completed   = elements.filter(_.complete)
    val remaining   = elements.filter(_.isRemaining)

    // Completions with a known time, in completion order.
    val timed: List[(Timestamp, Element)] =
      completed.flatMap(e => e.completionTime.map(_ -> e)).sortBy(_._1)

    val nights: Set[LocalObservingNight] =
      completed.flatMap(_.nights).toSet

    val remainingSites: Set[Site] =
      remaining.flatMap(_.remainingSites).toSet

    val countMet: Boolean =
      elements.size >= required && (
        if branch.ordered then elements.take(required).forall(_.complete)
        else completed.size >= required
      )

    def violation(t: GroupConstraintViolationType, es: List[Element]): GroupConstraintViolation =
      GroupConstraintViolation(t, gid, es.flatMap(_.observationIds), es.flatMap(_.subgroupIds))

    import GroupConstraintViolationType.*

    // Whether enough elements remain to reach the minimum.  An unschedulable
    // subgroup sinks the group only if the group needs it.
    val availability: List[GroupConstraintViolation] =
      val needed     =
        if branch.ordered then elements.take(required).filter(_.unschedulable)
        else if elements.count(!_.unschedulable) < required then elements.filter(_.unschedulable)
        else Nil
      List(
        Option.when(needed.nonEmpty)(violation(SubgroupUnschedulable, needed)),
        Option.when(elements.size < required)(violation(InsufficientElements, Nil))
      ).flatten

    // An element completed before an earlier one: either the earlier one is
    // still incomplete or it completed later.
    val order: List[GroupConstraintViolation] =
      if !branch.ordered then Nil
      else
        elements.zipWithIndex.collect {
          case (later, j) if later.complete =>
            elements.take(j).find: earlier =>
              !earlier.complete || (for
                te <- earlier.completionTime
                tl <- later.completionTime
              yield te > tl).getOrElse(false)
            .map(earlier => violation(Order, List(earlier, later)))
        }.flatten

    val intervals: List[GroupConstraintViolation] =
      timed.zip(timed.drop(1)).flatMap { case ((t0, e0), (t1, e1)) =>
        val gap = TimeSpan.between(t0, t1)
        List(
          Option.when(minInterval.exists(gap < _))(violation(MinimumInterval, List(e0, e1))),
          Option.when(maxInterval.exists(gap > _))(violation(MaximumInterval, List(e0, e1)))
        ).flatten
      }

    val sameNight: List[GroupConstraintViolation] =
      Option.when(branch.sameNight && nights.size > 1)(violation(SameNight, completed)).toList

    // Deadlines `now` has passed, for a group still short of completion.
    val deadlines: List[GroupConstraintViolation] =
      if countMet then Nil
      else
        val maxMissed  =
          for
            (t, e) <- timed.lastOption
            max    <- maxInterval
            if now > (t +| max)
          yield violation(MaximumInterval, List(e))
        val nightMissed =
          Option.when(
            branch.sameNight &&
            nights.size == 1 &&
            nightEnds(nights.head, remainingSites).forall(now >= _)
          )(violation(SameNight, completed))
        List(maxMissed, nightMissed).flatten

    val violations: List[GroupConstraintViolation] =
      availability ++ order ++ intervals ++ sameNight ++ deadlines

    val started: Boolean =
      elements.exists(_.started)

    val status: GroupStatus =
      if violations.nonEmpty then GroupStatus.Unschedulable
      else if countMet then GroupStatus.Complete
      else if started then GroupStatus.Ongoing
      else GroupStatus.Pending

    val isOpen: Boolean =
      status === GroupStatus.Pending || status === GroupStatus.Ongoing

    val candidates: List[Observation.Id] =
      if !isOpen then Nil
      else if branch.ordered then elements.find(_.isRemaining).toList.flatMap(_.candidates)
      else remaining.flatMap(_.candidates)

    val nextWindow: Option[GroupWindow] =
      for
        last  <- timed.lastOption.map(_._1)
        if status === GroupStatus.Ongoing
        if minInterval.isDefined || maxInterval.isDefined || branch.sameNight
      yield
        val nightEnd = Option.when(branch.sameNight && nights.size == 1)(
          nightEnds(nights.head, remainingSites).min
        )
        GroupWindow(
          start = minInterval.fold(last)(last +| _),
          end   = (maxInterval.map(last +| _).toList ++ nightEnd.toList).minOption
        )

    // When the group completed: the completion that met its minimum.
    val completionTime: Option[Timestamp] =
      Option.when(status === GroupStatus.Complete) {
        if branch.ordered then elements.take(required).flatMap(_.completionTime).maxOption
        else
          val ts = timed.map(_._1)
          ts.lift(required - 1).orElse(ts.lastOption)
      }.flatten

    val progress = GroupProgress(
      status             = status,
      completed          = NonNegInt.unsafeFrom(completed.size),
      required           = NonNegInt.unsafeFrom(required),
      nextWindow         = nextWindow,
      nextObservationIds = if branch.ordered then candidates else Nil,
      violations         = violations
    )

    val element = Element(
      observationIds = Nil,
      subgroupIds    = List(gid),
      excluded       = false,
      complete       = status === GroupStatus.Complete,
      unschedulable  = status === GroupStatus.Unschedulable,
      started        = started,
      completionTime = completionTime,
      nights         = nights,
      remainingSites = if isOpen then remainingSites else Set.empty,
      candidates     = candidates
    )

    (progress, element)
