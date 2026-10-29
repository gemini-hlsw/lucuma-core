// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import cats.data.State
import cats.syntax.all.*
import lucuma.core.util.DateInterval
import lucuma.core.data.PerSite
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Allocation
import lucuma.core.util.TimeSpan
import lucuma.core.enums.Site
import lucuma.core.util.Enumerated
import lucuma.core.data.Metadata
import lucuma.core.util

case class Chopper(cfpActive: DateInterval):

  case class ChopState(
    obsWorkingSet: PerSite[List[ItacObservation]],
    allocWorkingSet: Map[(TimeAccountingCategory, ScienceBand), TimeSpan]
  )

  def allocations(cat: TimeAccountingCategory, band: ScienceBand): State[ChopState, PerSite[Allocation]] =
    State: cs =>

      // We have this much time available and need to split it across both sites
      val availableTime = cs.allocWorkingSet.get((cat, band)).orEmpty

      if availableTime.isZero then
        // code below will work in this case, but we can exit early and save some pointless work
        (cs, PerSite.const(Allocation(cat, band, TimeSpan.Zero)))
      else

        // println(f"available time for $cat $band = ${availableTime.toHours}%3.1f")

        // Partition our working set into observable and non-observable for this cat and band.
        // This is where we filter out observations at sites where we're not allowed to use time.
        val partitions: PerSite[(List[ItacObservation], List[ItacObservation])] = 
          cs.obsWorkingSet.mapWithSite: (site, os) =>
            if cat.sites.contains(site) 
            then os.partition(_.isObservableInBand(band))
            else (Nil, os)

        // Total requested time for observable obs, per site
        val requestedTime: PerSite[TimeSpan] =
          partitions.map(_._1.foldMap(_.time))

        // println(s"requested times are ${requestedTime.map(_.toHours)}")

        // Total requested time for both sites
        val totalRequestedTime: TimeSpan = 
          requestedTime.combineAll

        // Percentage of available time to be allocated to each site.
        val ratios: PerSite[BigDecimal] = 
          if totalRequestedTime.isZero then
            PerSite.const(0.0)
          else
            requestedTime.map(_.toHours / totalRequestedTime.toHours)

        // println(s"ratios are ${ratios}")

        // This gives us what we need to split our allocation up proportionally.
        val ret = ratios.map: r =>
          Allocation(cat, band, availableTime *| r)

        // Now we need to update our partitions by subtracting allocated time.
        val newObsWorkingSet = partitions.mapWithSite: 
          case (site, (as, bs)) =>
            // println(s"newObsWorkingSet for $site:")

            // we need to reduce the time of as by the allocated time
            val requestedSiteTime = as.foldMap(_.time)
            val allocatedSiteTime = ret(site).duration

            // println(f"  requested and allocated times are ${requestedSiteTime.toHours}%3.2f ${allocatedSiteTime.toHours}%3.2f")

            val siteRatio: BigDecimal = 
              if requestedSiteTime.toHours == 0 
              then 0 
              else allocatedSiteTime.toHours / requestedSiteTime.toHours

            // println(f"  deduction is $siteRatio%3.2f")

            val as2 = as.map(o => o.copy(time = o.time -| (o.time *| siteRatio)))
            // if !as.corresponds(as2)((a, b) => a.time === b.time) then
            //   println(s"available time is ${availableTime.toHours}")
            //   println(s"$site $cat $band -> old times are ${as.map(_.time.toHours)}")
            //   println(s"$site $cat $band -> new times are ${as2.map(_.time.toHours)}")
            as2 ++ bs

        val newAllocWorkingSet =
          cs.allocWorkingSet - (cat -> band)

        (ChopState(newObsWorkingSet, newAllocWorkingSet), ret)
        
  def shards(p: Proposal)(using Metadata): (Site, TimeAccountingCategory, ScienceBand) => ProposalShard =

    // Our categories, with single-site ones first
    val cats = Enumerated[TimeAccountingCategory].all.sortBy(_.sites.size)

    // Initial working set, partitioned by site.
    val obsWorkingSet = PerSite.unfold(s => p.originalObsList.filter(_.isObservableAtSite(s, p.cfpActive)))

    // Initial allocation set
    val allocWorkingSet: Map[(TimeAccountingCategory, ScienceBand), TimeSpan] =
      p.allocations.toList.foldMap: a => 
        Map((a.category, a.scienceBand) -> a.duration)

    val map: List[PerSite[Allocation]] = 
      (cats, Enumerated[ScienceBand].all)
        .tupled
        .traverse(allocations)
        .runA(ChopState(obsWorkingSet, allocWorkingSet))
        .value

    (site, cat, band) =>
      ProposalShard(
        p,
        site,
        map.map(_.apply(site))
          .find: a =>
            a.category === cat && a.scienceBand === band
          .getOrElse:
            Allocation(cat, band, TimeSpan.Zero)
      )


