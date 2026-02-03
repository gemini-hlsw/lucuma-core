// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.model.Night
import lucuma.core.model.TwilightBoundedNight
import java.util.Date
import java.time.Instant
import lucuma.core.model.ObservingNight
import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import org.typelevel.cats.time.*

class NightIterator(site: Site, start: Date, end: Date) extends Iterator[Night]:
  val delegate = NightIterator.bounded(TwilightType.Nautical, site, start.toInstant(), end.toInstant())
  export delegate.{ next, hasNext }

object NightIterator: 

  /** An infinite iteration of `TwilightBoundedNight` of the specified type and starting night. */
  def from(tpe: TwilightType, observingNight: ObservingNight): Iterator[TwilightBoundedNight] =
    Iterator.unfold(observingNight): on =>
      TwilightBoundedNight.fromTwilightTypeAndObservingNight(tpe, on).tupleRight(on.next)

  /** 
   * An finite (possibly empty) iteration of `Night` with the specified hard boundaries; i.e. the first
   * and last nights may be trimmed. 
   */
  def bounded(tpe: TwilightType, site: Site, start: Instant, end: Instant): Iterator[Night] =
    BoundedInterval.closed(start, end) match
      case None => Iterator.empty
      case Some(bounds) =>    
        from(tpe, ObservingNight.fromSiteAndInstant(site, start))
          .map: n =>
            (n.interval & bounds).map: i =>
              println(i.toInterval.isEmpty)
              new Night:
                export n.site
                val interval = i
          .takeWhile(_.isDefined).map(_.get) // n.b. `collect` doesn't work on Iterators
          
