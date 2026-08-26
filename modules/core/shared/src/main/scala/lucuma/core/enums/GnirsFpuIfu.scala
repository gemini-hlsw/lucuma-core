// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.math.Angle
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS IFU FPU.
 * @group Enumerations (Generated)
 */
enum GnirsFpuIfu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Angle,
) derives Enumerated, Display:
  case LowResolution  extends GnirsFpuIfu("LowResolution",  "LR-IFU", "Low resolution IFU",  Angle.fromDoubleArcseconds(3.150))
  case HighResolution extends GnirsFpuIfu("HighResolution", "HR-IFU", "High resolution IFU", Angle.fromDoubleArcseconds(1.250))
