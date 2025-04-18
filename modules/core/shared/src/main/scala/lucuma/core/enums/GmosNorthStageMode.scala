// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS North stage modes.
 */
enum GmosNorthStageMode(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated:

  case NoFollow extends GmosNorthStageMode("NoFollow", "No Follow", "Do Not Follow")
  case FollowXy extends GmosNorthStageMode("FollowXy", "Follow XY", "Follow in XY")
