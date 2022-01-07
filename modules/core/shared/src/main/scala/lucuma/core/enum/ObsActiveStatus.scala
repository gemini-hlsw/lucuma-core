// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.`enum`

import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import monocle.Iso

/**
 * Determines whether an observation should be considered active.  It, for
 * example, "allows PIs to prevent or halt execution of "Ready'' or "Ongoing''
 * observations while retaining their status information".
 */
sealed abstract class ObsActiveStatus(val label: String, val toBoolean: Boolean) extends Product with Serializable {

  def fold[A](active: => A, inactive: => A): A =
    this match {
      case ObsActiveStatus.Active   => active
      case ObsActiveStatus.Inactive => inactive
    }

}

object ObsActiveStatus {

  case object Active   extends ObsActiveStatus("Active", true)
  case object Inactive extends ObsActiveStatus("Inactive", false)

  /** @group Typeclass Instances */
  implicit val EnumeratedObsActiveStatus: Enumerated[ObsActiveStatus] =
    Enumerated.of(
      Active,
      Inactive
    )

  implicit val DisplayObsActiveStatus: Display[ObsActiveStatus] =
    Display.byShortName(_.label)

  val FromBoolean: Iso[Boolean, ObsActiveStatus] =
    Iso[Boolean, ObsActiveStatus](b => if (b) Active else Inactive)(_.toBoolean)

}