// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.math.Voltage
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNRIS Well Depth.
 * @group Enumerations (Generated)
 */
enum GnirsWellDepth(
  val tag: String,
  val shortName: String,
  val longName: String,
  val bias_level: Voltage
) derives Enumerated, Display:
  case Shallow extends GnirsWellDepth("Shallow", "Shallow", "Shallow", Voltage.unsafeFromLongMillivolts(300))
  case Deep extends GnirsWellDepth("Deep", "Deep", "Deep", Voltage.unsafeFromLongMillivolts(600))

object GnirsWellDepth:
  // ATTENTION: This logic is duplicated in the DB view in the ODB. Modify it there too if it's changed here.
  def forCamera(camera: GnirsCamera): GnirsWellDepth =
    camera match // By default, use shallow for blue camera and deep for red camera.
      case GnirsCamera.ShortBlue | GnirsCamera.LongBlue => GnirsWellDepth.Shallow
      case GnirsCamera.ShortRed | GnirsCamera.LongRed   => GnirsWellDepth.Deep