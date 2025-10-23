// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for Guiding speed
 */
enum GuideSpeed(val tag: String) derives Enumerated:
  case Fast   extends GuideSpeed("fast")
  case Medium extends GuideSpeed("medium")
  case Slow   extends GuideSpeed("slow")

object GuideSpeed:
  val inSpeedOrder: List[GuideSpeed] = List(Fast, Medium, Slow)
