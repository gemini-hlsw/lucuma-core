// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.flamingos2

import algebra.instances.all.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.accepted.*
import lucuma.core.math.Angle
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.*

// Size of probe arm components in mm.
val PickoffMirrorSize          = BigDecimal(19800).withUnit[Micrometer]
val ProbePickoffArmTotalLength = BigDecimal(203400).withUnit[Micrometer]
val ProbeBaseArmLength         = BigDecimal(109630).withUnit[Micrometer]
val ProbePickoffArmLength      = ProbePickoffArmTotalLength - PickoffMirrorSize/2.0
val ProbeArmOffset             = BigDecimal(256870).withUnit[Micrometer]

// Width and length of tapered end of probe arm in arcsec.
val ProbeArmTaperedWidth       = 15000.mas
val ProbeArmTaperedLength      = 180000.mas

// Geometry features for Flamingos2, in mm.
val LongSlitFOVHeight   = BigDecimal(164100).withUnit[Micrometer]
val LongSlitFOVSouthPos = BigDecimal(112000).withUnit[Micrometer]
val LongSlitFOVNorthPos = BigDecimal(52100).withUnit[Micrometer]
val ImagingFOVSize      = BigDecimal(230120).withUnit[Micrometer]
val MOSFOVWidth         = BigDecimal(75160).withUnit[Micrometer]

// The diameter of the circle centered over the base position, in mm
val EntranceWindowRadius  = BigDecimal(139700).withUnit[Micrometer]
// The offsets of the patrol area circles, in mm
val BasePivotPoint        = BigDecimal(250593.4).withUnit[Micrometer]
val PickOffPivotPoint     = BasePivotPoint - BigDecimal(77665.3).withUnit[Micrometer]
// The diameter of the upper (smaller) patrol area circle, in mm
val UpperPatrolAreaRadius = BigDecimal(191028.6).withUnit[Micrometer]
// The diameter of the lower (larger) patrol area circle, in mm
val LowerPatrolAreaRadius = BigDecimal(268693.9).withUnit[Micrometer]
// The high limit of the bounding box to the right
val PatrolAreaHiLimit     = BigDecimal(113000).withUnit[Micrometer]

val Flamingos2PixelScale: PixelScale   = BigDecimal(0.18).withUnit[ArcSecondPerPixel]

def flamingos2SlitWidthPixels(slitWidth: Angle): Quantity[BigDecimal, Pixels] =
  val widthArcSeconds = Angle.decimalArcseconds.get(slitWidth).withUnit[ArcSecond]
  widthArcSeconds / Flamingos2PixelScale

object all
  extends Flamingos2ScienceAreaGeometry
  with Flamingos2CandidatesArea

object oiwfs:
  object patrolField extends Flamingos2OiwfsPatrolField
  object probeArm extends Flamingos2OiwfsProbeArm
