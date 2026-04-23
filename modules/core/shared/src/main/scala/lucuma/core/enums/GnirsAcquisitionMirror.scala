// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated


/**
 * Enumerated type for GNIRS Acquisition Mirror.
 * @group Enumerations (Generated)
 */
enum GnirsAcquisitionMirror(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated, Display:
    case In extends GnirsAcquisitionMirror("In", "In", "In")
    case Out extends GnirsAcquisitionMirror("Out", "Out", "Out")