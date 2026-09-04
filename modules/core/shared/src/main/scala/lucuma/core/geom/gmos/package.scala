// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import algebra.instances.all.given
import coulomb.*
import coulomb.syntax.*
import coulomb.units.accepted.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.math.syntax.units.*
import lucuma.core.math.units.*

val GmosPixelScale: PixelScale = 0.0807.pixelScale

val LongSlitHeight: Angle = 108000.mas

val NodAndShuffleHeight: Angle = 108000.mas

/**
 * Height of an IFU lenslet field, the same for one slit and two: masking to one pseudo-slit halves
 * the field across `p`, not along `q` (`GmosCommonType.IFU_FOV`).
 */
val IfuFieldHeight: Angle = 5000.mas

/**
 * Distance between the two IFU lenslet fields in the focal plane (`GmosCommonType.IFU_FOV_OFFSET`
 * either side of the pointing). The telescope points so the target field lands on the base, which
 * carries the sky field out to roughly this much in `p`.
 */
val IfuFieldSeparation: Angle = 60000.mas

def gmosSlitWidthPixels(slitWidth: Angle, xBin: GmosXBinning): Quantity[BigDecimal, Pixels] =
  val widthArcSeconds = Angle.decimalArcseconds.get(slitWidth).arcsecs
  widthArcSeconds / (BigDecimal(xBin.count.value) * GmosPixelScale)

private[gmos] def ifuOffset(fpu: Either[GmosNorthFpu, GmosSouthFpu]): Offset =
  fpu.fold(_.xOffset, _.xOffset).offsetInP

object all
  extends GmosScienceAreaGeometry
  with GmosCandidatesArea

object oiwfs:
  object patrolField extends GmosOiwfsPatrolField
  object probeArm extends GmosOiwfsProbeArm
