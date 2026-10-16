// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.Site
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

/**
 * The facts needed to evaluate group progress, as a tree of groups (branches)
 * and observations (leaves).  Deleted elements should be left out.
 */
sealed trait GroupProgressNode

object GroupProgressNode:

  /**
   * An observation.  `inactive` is true when a user has set it inactive (or
   * declined it, for a telluric).  `completionTime` is the time of its last
   * execution event once it is complete, if known.  `site` is where it would
   * be observed, if its instrument is known.
   */
  final case class Leaf(
    observationId:  Observation.Id,
    executionState: ExecutionState,
    inactive:       Boolean,
    completionTime: Option[Timestamp],
    site:           Option[Site]
  ) extends GroupProgressNode

  /**
   * A group.  A `minimumRequired` of `None` requires every viable element.
   * Zero intervals are treated as unset.
   */
  final case class Branch(
    groupId:         Group.Id,
    minimumRequired: Option[NonNegShort],
    ordered:         Boolean,
    minimumInterval: Option[TimeSpan],
    maximumInterval: Option[TimeSpan],
    sameNight:       Boolean,
    children:        List[GroupProgressNode]
  ) extends GroupProgressNode

  given Eq[GroupProgressNode] = Eq.fromUniversalEquals
