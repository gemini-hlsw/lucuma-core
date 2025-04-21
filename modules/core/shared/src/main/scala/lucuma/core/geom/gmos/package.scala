// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import algebra.instances.all.given
import coulomb.*
import coulomb.ops.algebra.spire.all.*
import coulomb.policy.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import lucuma.core.enums.GmosXBinning
import lucuma.core.math.Angle
import lucuma.core.math.units.*

val GmosPixelScale: PixelScale = BigDecimal(0.0807).withUnit[ArcSecondPerPixel]

def gmosSlitWidthPixels(slitWidth: Angle, xBin: GmosXBinning): Quantity[BigDecimal, Pixels] =
  val widthArcSeconds = Angle.decimalArcseconds.get(slitWidth).withUnit[ArcSecond]
  widthArcSeconds / (BigDecimal(xBin.count) * GmosPixelScale)

object all extends GmosOiwfsProbeArm with GmosScienceAreaGeometry with GmosCandidatesArea
