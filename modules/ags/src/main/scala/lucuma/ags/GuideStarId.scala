// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.core.model.Target

/**
 * What a guide star is.
 *
 * `Catalog` is a star from a catalog, identified by its catalog name, which carries the catalog and
 * its release. `ObservationTarget` is one of the observation's own targets (today a science target,
 * which the Altair AOWFS can guide on, on axis).
 *
 * A candidate built from a target always carries the target id. The id-less
 * `ObservationTarget(None)` only ever appears as a selection and means "the observation's only
 * science target".
 */
enum GuideStarId:
  case Catalog(name: GuideStarName)
  case ObservationTarget(targetId: Option[Target.Id])

  /** None for an observation target. */
  def catalogName: Option[GuideStarName] =
    this match
      case GuideStarId.Catalog(name)        => name.some
      case GuideStarId.ObservationTarget(_) => none

  /** None for an observation target and for a catalog name without a Gaia source id. */
  def gaiaSourceId: Option[Long] =
    catalogName.flatMap(_.toGaiaSourceId)

  /** None for a catalog star and for the id-less observation target. */
  def observationTargetId: Option[Target.Id] =
    this match
      case GuideStarId.Catalog(_)                  => none
      case GuideStarId.ObservationTarget(targetId) => targetId

  def isObservationTarget: Boolean =
    this match
      case GuideStarId.Catalog(_)           => false
      case GuideStarId.ObservationTarget(_) => true

  /** Whether this id, as a selection, picks the candidate with id `candidate`. */
  def selects(candidate: GuideStarId): Boolean =
    (this, candidate) match
      case (GuideStarId.Catalog(a), GuideStarId.Catalog(b))                                 => a === b
      case (GuideStarId.ObservationTarget(None), GuideStarId.ObservationTarget(_))          => true
      case (GuideStarId.ObservationTarget(Some(a)), GuideStarId.ObservationTarget(Some(b))) =>
        a === b
      case _                                                                                => false

object GuideStarId:
  /** A Gaia DR3 star from its source id. */
  def gaia(sourceId: Long): GuideStarId =
    Catalog(GuideStarName.gaiaSourceId.reverseGet(sourceId))

  // Observation targets sort first: the id is the last tiebreak in the AGS ranking, so an on-axis
  // target wins an exact tie. Catalog stars sort by Gaia source id numerically, then by name.
  given Order[GuideStarId] =
    Order.by:
      case GuideStarId.ObservationTarget(targetId) =>
        (0, targetId, none[Long], "")
      case GuideStarId.Catalog(name)               =>
        (1, none[Target.Id], name.toGaiaSourceId, name.value.value)
