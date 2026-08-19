// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.math.Angle
import lucuma.core.math.syntax.int.*
import lucuma.core.util.Enumerated

enum GmosFpuType(val tag: String) derives Enumerated:
  case LongSlit extends GmosFpuType("LongSlit")
  case Ns       extends GmosFpuType("Ns")
  case Ifu      extends GmosFpuType("Ifu")

  /**
   * Extent along q of a GMOS slit of this type, which depends only on the type
   * and not on the slit width. `None` for IFUs, whose field size is not a slit
   * length (their `effectiveSlitWidth` is the pseudo-slit width, not the field).
   */
  def slitLength: Option[Angle] =
    this match
      case LongSlit => Some(GmosFpuType.LongSlitLength)
      case Ns       => Some(GmosFpuType.NodAndShuffleLength)
      case Ifu      => None

object GmosFpuType:

  // Three 108" sections separated by two 3.2" gaps; see GmosScienceAreaGeometry.
  val LongSlitLength: Angle = 330400.mas

  val NodAndShuffleLength: Angle = 108000.mas
