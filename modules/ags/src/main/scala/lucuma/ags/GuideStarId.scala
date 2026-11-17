// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order
import cats.syntax.all.*
import lucuma.core.model.Target

/**
 * What a guide star is.
 *
 * A catalog star is identified by its source id within one catalog release, one case per release:
 * source ids are not stable across releases. `ObservationTarget` is one of the observation's own
 * targets (today a science target, which the Altair AOWFS can guide on, on axis).
 *
 * A candidate built from a target always carries the target id. The id-less
 * `ObservationTarget(None)` only ever appears as a selection and means "the observation's only
 * science target".
 */
enum GuideStarId:
  case GaiaDr3(sourceId: Long)
  case ObservationTarget(targetId: Option[Target.Id])

  /**
   * The catalog name, which carries the catalog and its release. None for an observation target.
   */
  def catalogName: Option[GuideStarName] =
    this match
      case GuideStarId.GaiaDr3(sourceId)    => GuideStarName.gaiaSourceId.reverseGet(sourceId).some
      case GuideStarId.ObservationTarget(_) => none

  /** None for an observation target. */
  def gaiaSourceId: Option[Long] =
    this match
      case GuideStarId.GaiaDr3(sourceId)    => sourceId.some
      case GuideStarId.ObservationTarget(_) => none

  /** None for a catalog star and for the id-less observation target. */
  def observationTargetId: Option[Target.Id] =
    this match
      case GuideStarId.GaiaDr3(_)                  => none
      case GuideStarId.ObservationTarget(targetId) => targetId

  def isObservationTarget: Boolean =
    this match
      case GuideStarId.GaiaDr3(_)           => false
      case GuideStarId.ObservationTarget(_) => true

  /** Whether this id, as a selection, picks the candidate with id `candidate`. */
  def selects(candidate: GuideStarId): Boolean =
    (this, candidate) match
      case (GuideStarId.GaiaDr3(a), GuideStarId.GaiaDr3(b))                                 => a === b
      case (GuideStarId.ObservationTarget(None), GuideStarId.ObservationTarget(_))          => true
      case (GuideStarId.ObservationTarget(Some(a)), GuideStarId.ObservationTarget(Some(b))) =>
        a === b
      case _                                                                                => false

object GuideStarId:
  /** None when the source id in the name does not fit a Long. */
  def fromCatalogName(name: GuideStarName): Option[GuideStarId] =
    name.toGaiaSourceId.map(GaiaDr3(_))

  // Observation targets sort first: the id is the last tiebreak in the AGS ranking, so an on-axis
  // target wins an exact tie. Catalog stars sort by source id.
  given Order[GuideStarId] =
    Order.by:
      case GuideStarId.ObservationTarget(targetId) => (0, targetId, 0L)
      case GuideStarId.GaiaDr3(sourceId)           => (1, none[Target.Id], sourceId)
