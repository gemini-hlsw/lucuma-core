// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.data.Metadata
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.TooActivation
import lucuma.core.model.Allocation
import lucuma.core.model.IntPercent
import lucuma.core.model.ProposalReference
import lucuma.core.model.ProposalType
import lucuma.core.util.DateInterval
import lucuma.core.util.TimeSpan

import java.time.LocalDate

import GroupTree.flattenAndScale
import lucuma.core.util.Enumerated
import lucuma.core.model.ProgramReference
import lucuma.core.enums.ScienceSubtype

import lucuma.core.enums.Observatory
import java.io.File

case class Proposal(
  xmlFile: Option[File],
  reference: ProposalReference,
  piName: String,
  allocations: NonEmptyList[Allocation],
  tpe: ProposalType = ProposalType.Queue(TooActivation.None, IntPercent.unsafeFrom(0), Nil), // TODO
  groupTree: GroupTree[ItacObservation] = GroupTree.empty,
  cfpActive: DateInterval = DateInterval.between(LocalDate.now(), LocalDate.now()),
  ranking: PosDouble = PosDouble.MaxValue // defaulted to avoid having to change all the tests
) {

  /** All observations, scaled only for group membership (not for allocations). */
  val originalObsList: List[ItacObservation] =
    groupTree.flattenAndScale

  private def scienceSubtype: ScienceSubtype =
    tpe match
      case ProposalType.Classical(_, _)            => ScienceSubtype.Classical
      case ProposalType.DemoScience(_, _)          => ScienceSubtype.DemoScience
      case ProposalType.DirectorsTime(_, _)        => ScienceSubtype.DirectorsTime
      case ProposalType.FastTurnaround(_, _, _, _) => ScienceSubtype.FastTurnaround
      case ProposalType.LargeProgram(_, _, _, _)   => ScienceSubtype.LargeProgram
      case ProposalType.PoorWeather                => ScienceSubtype.PoorWeather
      case ProposalType.Queue(_, _, _)             => ScienceSubtype.Queue
      case ProposalType.SystemVerification(_, _)   => ScienceSubtype.SystemVerification

  private def observatory: Observatory =
    originalObsList.map(_.mode.observatory).distinct match
      case a :: Nil => a
      case other => sys.error(s"Unpossible: proproposal $reference doesn't have exactly one observatory: ${other.mkString("[", ", ", "]")}")

  def programReference: ProgramReference =
    observatory match
      case Observatory.Gemini => ProgramReference.Science(reference, scienceSubtype)
      case Observatory.Keck   => ProgramReference.Keck(reference)
      case Observatory.Subaru => ProgramReference.Subaru(reference, ???)
    
  ///
  /// SHARDS
  ///

  // In order to compute a shard, we need to take site affinity into account.
  //
  // Strategy 1:
  // - Scale all observations based on total recommended time, without taking site affinity into account.
  //   Call this the working set.
  // - Create the shards for single-site time accounting categories, assigning all time to those sites,
  //   subtracting that time evenly from corresponding site-affiliated observations in the working set.
  // - Create shards for dual-site time accounting categories, dividing the time evenly across the working
  //   set, which should be empty when we're done.
  
  val shardFor = Chopper(cfpActive).shards(this)(using Metadata.placeholder)

  // def shardFor(site: Site, category: TimeAccountingCategory, band: ScienceBand): ProposalShard =
    // val alloc = 
    //   if category.sites.contains(site) then allocationFor(category, band)
    //   else Allocation(category, band, TimeSpan.Zero)
    // ProposalShard(
    //   this,
    //   site,
    //   alloc
    // )

  def shards: List[ProposalShard] =
    (Enumerated[Site].all, Enumerated[TimeAccountingCategory].all, Enumerated[ScienceBand].all)
      .mapN(shardFor(_, _, _))
      .filter(_.nonEmpty)

  def too: TooActivation =
    ProposalType.TooActivation.getOption(tpe).getOrElse(TooActivation.None)

  ///
  /// ALLOCATIONS
  ///

  /** 
   * The possibly empty allocation for the specified category and band. There should never be more than
   * one of these but we handle this case and combine the result.
   */
  // private def allocationFor(category: TimeAccountingCategory, band: ScienceBand): Allocation =
  //   Allocation(
  //     category,
  //     band,
  //     allocations
  //       .collectFold: 
  //         case Allocation(`category`, `band`, duration) => duration
  //   )
 
  // /** Allocations awarded in the specified accounting category. */
  // private def allocationsForTimeAccountingCategory(category: TimeAccountingCategory): List[Allocation] =
  //   allocations.filter(_.category === category)

  /** Allocations that can be used at the specified site, filtered for the given band. */
  private def allocationsForSiteAndBand(site: Site, band: ScienceBand): List[Allocation] =
    allocations.filter:
      case Allocation(TimeAccountingCategory.UH, `band`, _) => site == Site.GN
      case Allocation(TimeAccountingCategory.CL, `band`, _) => site == Site.GS
      case Allocation(_, `band`, _)                         => true
      case _                                                => false

  ///
  /// ALLOCATED TIME
  ///

  // private def allocatedTime: TimeSpan =
  //   allocations.foldMap(_.duration)

  // private def allocatedTimeForTimeAccountingCategory(category: TimeAccountingCategory): TimeSpan =
  //   allocationsForTimeAccountingCategory(category).foldMap(_.duration)

  ///
  /// USABLE TIME
  ///

  /** Total time usable at the specified site, in the given band. In most cases time can be used at either site. */
  private def usableTimeForSiteAndBand(site: Site, band: ScienceBand): TimeSpan =
    allocationsForSiteAndBand(site, band).foldMap(_.duration)

  ///
  /// OBSERVATIONS
  ///

  private def itacObservations: List[ItacObservation] =
    originalObsList

  /** 
   * The subset of observations observable at the specified site, in the specified band, with
   * their times scaled proportionally based on allocated time in the specified time/band.
   */
  def itacObservationsScaledForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): List[ItacObservation.Scaled] =
    val f = scaleFactorForSiteAndBand(site, band) 
    if f == 0 then Nil else
      itacObservationsForSiteAndBand(site, band).map: o =>
        ItacObservation.Scaled(o.copy(time = o.time *| f))

  /** Estimated time required for all observations at the specified site, in the given band. */
  private def estimatedTimeForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): TimeSpan =
    itacObservationsForSiteAndBand(site, band).foldMap(_.time)

  /** Factor by which original estimated times must be multiplied to yield the scaled time used for bucket-filling. */
  private def scaleFactorForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): BigDecimal =
    val estimated = estimatedTimeForSiteAndBand(site, band).toHours
    val awarded   = usableTimeForSiteAndBand(site, band).toHours 
    // if estimated > 0 then awarded / estimated else 0
    if estimated > 0 then (awarded / estimated) min 1.0 else 0

  /** Subset of observations observable at the specified site, in the specified band. */    
  private def itacObservationsForSiteAndBand(site: Site, band: ScienceBand)(using Metadata): List[ItacObservation] =
    itacObservations.filter: o =>
      o.isObservableAtSite(site, cfpActive) && o.isObservableInBand(band)

  override def toString(): String =
    s"""|Proposal:
        |   reference: $reference
        |   tpe: $tpe
        |   cfpActive: $cfpActive
        |   allocations:
        |     ${allocations.toList.mkString("\n     ")}
        |   observations (scaled):
        |     ${originalObsList.mkString("\n     ")}
        |""".stripMargin

}

object Proposal:

  def forTesting(
    reference: ProposalReference,
    allocations: NonEmptyList[Allocation],
    tpe: ProposalType = ProposalType.Queue(TooActivation.None, IntPercent.unsafeFrom(0), Nil), // TODO
    groupTree: GroupTree[ItacObservation] = GroupTree.empty,
    cfpActive: DateInterval = DateInterval.between(LocalDate.now(), LocalDate.now()),
    ranking: PosDouble = PosDouble.MaxValue // defaulted to avoid having to change all the tests
  ): Proposal =
    apply(None, reference, "Unknown", allocations, tpe, groupTree, cfpActive, ranking)