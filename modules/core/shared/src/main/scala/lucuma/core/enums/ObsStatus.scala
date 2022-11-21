// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class ObsStatus(val label: String) extends Product with Serializable

object ObsStatus {
  case object New       extends ObsStatus("New")
  case object Included  extends ObsStatus("Included")
  case object Proposed  extends ObsStatus("Proposed")
  case object Approved  extends ObsStatus("Approved")
  case object ForReview extends ObsStatus("For Review")
  case object Ready     extends ObsStatus("Ready")
  case object Ongoing   extends ObsStatus("Ongoing")
  case object Observed  extends ObsStatus("Observed")

  /** @group Typeclass Instances */
  implicit val ObsStatusEnumerated: Enumerated[ObsStatus] =
    Enumerated.of(
      New,
      Included,
      Proposed,
      Approved,
      ForReview,
      Ready,
      Ongoing,
      Observed
    )

  implicit val ObsStatusDisplay: Display[ObsStatus] =
    Display.byShortName(_.label)
}
