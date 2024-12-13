// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.syntax

import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.core.util.DateInterval

import java.time.Duration
import java.time.Instant

trait SiteOps:
  extension(site: Site)
    def midpoint(active: DateInterval): Instant =
      val start    = ObservingNight.fromSiteAndLocalDate(site, active.start).start
      val end      = ObservingNight.fromSiteAndLocalDate(site, active.end).end
      val duration = Duration.between(start, end)
      start.plus(duration.dividedBy(2L))

object site extends SiteOps
