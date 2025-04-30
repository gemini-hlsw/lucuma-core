// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data

import cats.collections.Diet
import cats.collections.Range
import cats.kernel.Eq
import cats.kernel.Semigroup
import cats.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.instances.instant.given
import lucuma.core.model.Semester
import lucuma.core.util.DateInterval

import java.time.Instant

/**
 * Wrapper for a `DisjointIntervalMap[Site, Instant]` with convenience methods for determining
 * resource availability for a given site, time [range], or both.
 */
final case class Availability(toDisjointIntervalMap: DisjointIntervalMap[Site, Instant]):

  /** The interval set for the given site. */
  def forSite(site: Site): Diet[Instant] = 
    toDisjointIntervalMap.get(site).getOrElse(Diet.empty)
  
  /** This `Availability`, narrowed to the specified range. */
  def forRange(range: Range[Instant]): Availability =
    Availability:
      toDisjointIntervalMap.intersect(range)

  /** This `Availability`, narrowed to the specified semester. */
  def forSemester(semester: Semester): Availability =
    Availability:
      DisjointIntervalMap.unsafeFromMap:
        Site.all.fproduct(forSiteAndSemester(_, semester)).toMap

  /** This `Availability`, narrowed to the specified DateInterval. */
  def forDateInterval(dateInterval: DateInterval): Availability =
    Availability:
      DisjointIntervalMap.unsafeFromMap:
        Site.all.fproduct(forSiteAndDateInterval(_, dateInterval)).toMap  
  
  /** The interval set for the given site and DateInterval. */
  def forSiteAndDateInterval(site: Site, dateInterval: DateInterval): Diet[Instant] =
    val start = dateInterval.start.atStartOfDay(site.timezone).toInstant() 
    val end   = dateInterval.end.atStartOfDay(site.timezone).plusDays(1).toInstant()
    forSiteAndRange(site, Range(start, end))

  /** The interval set for the given site and semester. */
  def forSiteAndSemester(site: Site, semester: Semester): Diet[Instant] =
    val start = semester.start.atSite(site).toInstant()
    val end   = semester.end.atSite(site).toInstant()      
    forSiteAndRange(site, Range(start, end))

  /** The interval set for the given site, intersected with the specified range. */
  def forSiteAndRange(site: Site, range: Range[Instant]): Diet[Instant] =
    forSite(site) & range

  /** The site whose interval set includes the specified instant (if any). */
  def siteForInstant(when: Instant): Option[Site] =
    toDisjointIntervalMap.getKeyForValue(when)

  /** The site whose interval set includes the *entire* specified range (if any). */
  def siteForRange(range: Range[Instant]): Option[Site] =
    toDisjointIntervalMap.getKeyForRange(range)

  /** This `Availability` intersected with `other`. */
  def intersect(other: Availability): Availability =
    Availability:
      toDisjointIntervalMap.intersect(other.toDisjointIntervalMap)

object Availability:

  /** The `Availability` that is completely empty for both sites. */
  val Never: Availability =
    Availability(DisjointIntervalMap.empty)

  /** Construct an `Availability` that is full for the specified site (and empty for the other). */
  def always(site: Site): Availability =
    Availability(DisjointIntervalMap.full(site))

  /** Availability forms a semigroup with intersection as the combining operation. */
  given Semigroup[Availability] =
    Semigroup.instance(_ intersect _)

  /** Availabilities can be compared. */
  given Eq[Availability] =
    Eq.by(_.toDisjointIntervalMap)

