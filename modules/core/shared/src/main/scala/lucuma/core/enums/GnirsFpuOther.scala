// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.util.Display
import lucuma.core.util.Enumerated


/**
 * Enumerated type for GNIRS FPU Other.
 * @group Enumerations (Generated)
 */
enum GnirsFpuOther(
  val tag: String,
  val shortName: String,
  val longName: String,
) derives Enumerated, Display:
  case Acquisition extends GnirsFpuOther("Acquisition", "Acquisition", "Acquisition")
  case PupilViewer extends GnirsFpuOther("PupilViewer", "Pupil", "Pupil viewer")
  case Pinhole1 extends GnirsFpuOther("Pinhole1", "Small pin", "pinhole 0.1")
  case Pinhole3 extends GnirsFpuOther("Pinhole3", "Large pin", "pinhole 0.3")
