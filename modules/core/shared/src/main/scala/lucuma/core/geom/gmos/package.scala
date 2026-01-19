// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.math.syntax.units.*
import lucuma.core.math.units.*

val GmosPixelScale: PixelScale = 0.0807.pixelScale

def gmosSlitWidthPixels(slitWidth: Angle, xBin: GmosXBinning): Quantity[BigDecimal, Pixels] =
  val widthArcSeconds = Angle.decimalArcseconds.get(slitWidth).arcsecs
  widthArcSeconds / (BigDecimal(xBin.count.value) * GmosPixelScale)

private[gmos] def ifuOffset(fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]]): Offset =
  fpu.fold(Angle.Angle0)(_.fold(_.xOffset, _.xOffset)).offsetInP

object all
  extends GmosScienceAreaGeometry
  with GmosCandidatesArea

object oiwfs:
  object patrolField extends GmosOiwfsPatrolField
  object probeArm extends GmosOiwfsProbeArm
