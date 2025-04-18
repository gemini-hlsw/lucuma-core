// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS South stage mode.
 */
enum GmosSouthStageMode(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated:

  case NoFollow  extends GmosSouthStageMode("NoFollow",  "No Follow",  "Do Not Follow")
  case FollowXyz extends GmosSouthStageMode("FollowXyz", "Follow XYZ", "Follow in XYZ(focus)")
  case FollowZ   extends GmosSouthStageMode("FollowZ",   "Follow Z",   "Follow in Z Only")
