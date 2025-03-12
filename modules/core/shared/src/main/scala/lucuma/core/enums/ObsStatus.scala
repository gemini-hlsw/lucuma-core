// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum ObsStatus(val tag: String, val label: String) derives Enumerated:
  case New       extends ObsStatus("new", "New")
  case Included  extends ObsStatus("included", "Included")
  case Proposed  extends ObsStatus("proposed", "Proposed")
  case Approved  extends ObsStatus("approved", "Approved")
  case ForReview extends ObsStatus("for_review", "For Review")
  case Ready     extends ObsStatus("ready", "Ready")
  case Ongoing   extends ObsStatus("ongoing", "Ongoing")
  case Observed  extends ObsStatus("observed", "Observed")

object ObsStatus:
  given Display[ObsStatus] = Display.byShortName(_.label)
