// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class ObsStatus(val tag: String, val label: String) extends Product with Serializable

object ObsStatus {
  case object New       extends ObsStatus("new", "New")
  case object Included  extends ObsStatus("included", "Included")
  case object Proposed  extends ObsStatus("proposed", "Proposed")
  case object Approved  extends ObsStatus("approved", "Approved")
  case object ForReview extends ObsStatus("for_review", "For Review")
  case object Ready     extends ObsStatus("ready", "Ready")
  case object Ongoing   extends ObsStatus("ongoing", "Ongoing")
  case object Observed  extends ObsStatus("observed", "Observed")

  /** @group Typeclass Instances */
  implicit val ObsStatusEnumerated: Enumerated[ObsStatus] =
    Enumerated.from(
      New,
      Included,
      Proposed,
      Approved,
      ForReview,
      Ready,
      Ongoing,
      Observed
    ).withTag(_.tag)

  implicit val ObsStatusDisplay: Display[ObsStatus] =
    Display.byShortName(_.label)
}
