// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import cats.kernel.Order
import cats.syntax.all.*
import lucuma.core.data.Metadata
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.ToOActivation
import lucuma.core.model.Allocation
import lucuma.core.model.ProposalReference
import lucuma.core.model.ProposalType
import lucuma.core.util.TimeSpan

/**
 * The slice of a proposal relevant to a specific site and allocation, with observations filtered
 * by site and time-scaled by the allocation size.
 */
class ProposalShard(
  parentProposal: Proposal,
  val site: Site,
  val allocation: Allocation,
):
  opaque type Observation <: ItacObservation.Scaled = ItacObservation.Scaled

  export parentProposal.tpe

  val reference: ProposalShard.Reference =
    ProposalShard.Reference(
      parentProposal.reference,
      allocation.category,
      allocation.scienceBand,
      site
    )

  def too: ToOActivation =
    ToOActivation.None // TODO

  /** 
   * Empty if the allocation is empty, otherwise all observations observable at `site`, scaled to
   * the allocation.
   */
  val observations: List[Observation] =
    if allocation.duration.isEmpty then Nil
    else parentProposal.itacObservationsScaledForSiteAndBand(site, allocation.scienceBand)(using Metadata.placeholder)

object ProposalShard:
  case class Reference(
    parentReference: ProposalReference, 
    category: TimeAccountingCategory,
    band: ScienceBand,
    site: Site
  ):
    override def toString(): String =
      s"$parentReference (Shard for $category/$band/$site)"
    
  object Reference:
    given Order[Reference] =
      Order.by: r => 
        (r.parentReference, r.category, r.band, r.site)