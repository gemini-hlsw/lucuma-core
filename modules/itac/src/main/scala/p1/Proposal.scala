// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ToOActivation
import lucuma.core.model.ProposalReference
import lucuma.core.util.TimeSpan
import cats.syntax.all.*
import cats.data.NonEmptyList
import lucuma.core.model.Allocation
import lucuma.core.model.ProposalType
import lucuma.core.model.IntPercent
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.DateInterval
import lucuma.core.data.Metadata

case class Proposal(
  reference: ProposalReference,
  allocations: NonEmptyList[Allocation],
  tpe: ProposalType = ProposalType.Queue(ToOActivation.None, IntPercent.unsafeFrom(0), Nil), // TODO
  obsList: List[ItacObservation] = Nil,
  band3Observations: List[ItacObservation] = Nil,
  cfpActive: DateInterval = null,
) {

  def too: ToOActivation =
    ProposalType.ToOActivation.getOption(tpe).getOrElse(ToOActivation.None)

  def obsListFor(band: ScienceBand): List[ItacObservation] =
    if (band == ScienceBand.Band3) band3Observations else obsList

  @deprecated
  def ntac = allocations.head

  /**
   * Gets the time for the proposal as a whole.
   */
  def time: TimeSpan = allocations.foldMap(_.duration)





  // new API below

  ///
  /// ALLOCATIONS
  ///

  /** Allocations awarded in the specified accounting category. */
  def allocationsForTimeAccountingCategory(category: TimeAccountingCategory): List[Allocation] =
    allocations.filter(_.category === category)

  /** Allocations that can be used at the specified site, filtered for the given band. */
  def allocationsForSiteAndBand(site: Site, band: ScienceBand): List[Allocation] =
    allocations.filter:
      case Allocation(TimeAccountingCategory.UH, `band`, _) => site == Site.GN
      case Allocation(TimeAccountingCategory.CL, `band`, _) => site == Site.GS
      case Allocation(_, `band`, _)                         => true
      case _                                                => false

  ///
  /// ALLOCATED TIME
  ///

  def allocatedTime: TimeSpan =
    allocations.foldMap(_.duration)

  def allocatedTimeForTimeAccountingCategory(category: TimeAccountingCategory): TimeSpan =
    allocationsForTimeAccountingCategory(category).foldMap(_.duration)

  ///
  /// USABLE TIME
  ///

  /** Total time usable at the specified site, in the given band. In most cases time can be used at either site. */
  def usableTimeForSiteAndBand(site: Site, band: ScienceBand): TimeSpan =
    allocationsForSiteAndBand(site, band).foldMap(_.duration)

  ///
  /// OBSERVATIONS
  ///

  def itacObservations: List[ItacObservation] =
    ???

  /** 
   * The subset of observations observable at the specified site, in the specified band, with
   * their times scaled proportionally based on allocated time in the specified time/band.
   */
  def itacObservationsScaledForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): List[ItacObservation.Scaled] =
    val f = scaleFactorForSiteAndBand(site, band)
    itacObservationsForSiteAndBand(site, band).map: o =>
      ItacObservation.Scaled(o.copy(time = time *| f))

  /** Estimated time required for all observations at the specified site, in the given band. */
  def estimatedTimeForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): TimeSpan =
    itacObservationsForSiteAndBand(site, band).foldMap(_.time)

  /** Factor by which original estimated times must be multiplied to yield the scaled time used for bucket-filling. */
  def scaleFactorForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): BigDecimal =
    estimatedTimeForSiteAndBand(site, band).toHours / usableTimeForSiteAndBand(site, band).toHours

  /** Subset of observations observable at the specified site, in the specified band. */    
  def itacObservationsForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): List[ItacObservation] =
    itacObservations.filter: o =>
      o.isObservableAtSite(site, cfpActive) && o.isObservableInBand(band)

}

