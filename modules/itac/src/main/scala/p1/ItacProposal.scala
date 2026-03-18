// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import cats.syntax.all.*
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.ToOActivation
import lucuma.core.model.Allocation
import lucuma.core.model.ProposalReference
import lucuma.core.model.ProposalType
import lucuma.core.util.TimeSpan 

// ok what do we really have?
case class ItacProposal(
  reference: ProposalReference,
  tpe: ProposalType,
  allocations: List[Allocation],
  itacObservations: List[ItacObservation]
):

  def toOActivation: Option[ToOActivation] =
    ProposalType.ToOActivation.getOption(tpe)

  def itacObservationsScaledForSiteAndBand(site: Site, band: ScienceBand): List[ItacObservation.Scaled] =
    ???

  /** Allocations that can be used at the given site, in the given band. */
  def allocatedTimeForSiteAndBand(site: Site, band: ScienceBand): TimeSpan =
    allocationsForSiteAndBand(site, band).foldMap(_.duration)

  /** Allocations that can be used at the given site, in the given band. */
  def allocationsForSiteAndBand(site: Site, band: ScienceBand): List[Allocation] =
    allocations.filter:
      case Allocation(TimeAccountingCategory.UH, `band`, _) => site == Site.GN
      case Allocation(TimeAccountingCategory.CL, `band`, _) => site == Site.GS
      case Allocation(_, `band`, _)                         => true
      case _                                                => false
      
