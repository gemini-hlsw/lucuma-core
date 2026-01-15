// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.offsets

import lucuma.core.math.Offset
import lucuma.core.util.NewType

/**
 * Position of an offset marker after rotation by position angle.
 */
object RotatedOffset extends NewType[Offset]
type RotatedOffset = RotatedOffset.Type
