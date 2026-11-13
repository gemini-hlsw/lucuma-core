// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import lucuma.core.enums.GroupConstraintViolationCode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight

/**
 * One particular violation of a group's constraints: its kind
 * (`GroupConstraintViolationCode`) and where in the group it happened.  The
 * location tells a violation apart from others of the same kind, so that each
 * may be waived on its own (see `GroupConstraintWaiver`).
 *
 * Only `Waivable` violations may be waived.  `Unwaivable` ones are fixed
 * instead.
 *
 * A group element is identified by `Left` of its group id when it is a
 * subgroup, or `Right` of its observation id.
 */
sealed trait GroupConstraintViolation:

  /** The kind of violation. */
  def code: GroupConstraintViolationCode

object GroupConstraintViolation:

  /** A violation that a waiver may accept. */
  enum Waivable extends GroupConstraintViolation:

    /**
     * In an ordered group, `element` completed before an earlier element: one
     * still incomplete, or completed later.
     */
    case Order(element: Either[Group.Id, Observation.Id])

    /**
     * An element started before the minimum interval had passed since the
     * completion of `after`, the element completed before it.
     */
    case MinimumInterval(after: Either[Group.Id, Observation.Id])

    /**
     * The next element after `after` completed later than the maximum
     * interval after it, or the maximum interval since its completion has
     * passed without another.
     */
    case MaximumInterval(after: Either[Group.Id, Observation.Id])

    /**
     * A same-night group's completions on `night` didn't finish it:
     * completions followed on a later night, or the night ended (or would end
     * before the minimum interval passed) with elements still to observe.
     */
    case SameNight(night: ObservingNight)

    /**
     * `night`, on which at least one of the group's observations completed,
     * ended with fewer than the group's minimum observations per night.
     */
    case ObservationsPerNightMinimum(night: ObservingNight)

    /**
     * More than the group's maximum observations per night completed on
     * `night`.
     */
    case ObservationsPerNightMaximum(night: ObservingNight)

    override def code: GroupConstraintViolationCode =
      this match
        case Order(_)                       => GroupConstraintViolationCode.Order
        case MinimumInterval(_)             => GroupConstraintViolationCode.MinimumInterval
        case MaximumInterval(_)             => GroupConstraintViolationCode.MaximumInterval
        case SameNight(_)                   => GroupConstraintViolationCode.SameNight
        case ObservationsPerNightMinimum(_) => GroupConstraintViolationCode.ObservationsPerNightMinimum
        case ObservationsPerNightMaximum(_) => GroupConstraintViolationCode.ObservationsPerNightMaximum

  /**
   * A violation fixed rather than waived: observations at more than one site
   * by moving them to one site, too few elements by setting a minimum or
   * adding a replacement, and an unschedulable subgroup by fixing or waiving
   * its own violations.
   */
  enum Unwaivable extends GroupConstraintViolation:

    /** A same-night group has observations at more than one site. */
    case SameNightMultipleSites

    /**
     * A group with observations-per-night limits has observations at more
     * than one site, so its nights are ambiguous.
     */
    case ObservationsPerNightMultipleSites

    /** Too few elements remain to complete the group. */
    case InsufficientElements

    /** `subgroup`, which the group needs, can no longer be completed. */
    case SubgroupUnschedulable(subgroup: Group.Id)

    override def code: GroupConstraintViolationCode =
      this match
        case SameNightMultipleSites            => GroupConstraintViolationCode.SameNightMultipleSites
        case ObservationsPerNightMultipleSites => GroupConstraintViolationCode.ObservationsPerNightMultipleSites
        case InsufficientElements              => GroupConstraintViolationCode.InsufficientElements
        case SubgroupUnschedulable(_)          => GroupConstraintViolationCode.SubgroupUnschedulable

  object Waivable:
    given Eq[Waivable] = Eq.fromUniversalEquals

  object Unwaivable:
    given Eq[Unwaivable] = Eq.fromUniversalEquals

  given Eq[GroupConstraintViolation] =
    Eq.fromUniversalEquals
