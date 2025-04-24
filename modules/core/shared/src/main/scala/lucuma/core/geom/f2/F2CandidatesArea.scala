// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.f2

import coulomb.policy.spire.standard.given
import coulomb.units.accepted.*
import lucuma.core.enums.F2LyotWheel
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import spire.std.bigDecimal.*

/**
  * F2 area that could be reachable by the patrol arm
  */
trait F2CandidatesArea:

  /**
    * F2 area where the probe arm can reach centered at 0
    */
  def candidatesArea(l: F2LyotWheel): ShapeExpression =
    // app 3.7 arcmin radius
    val diameter = EntranceWindowRadius * l.plateScale
    ShapeExpression.centeredEllipse(
      (diameter.toUnit[ArcSecond] * 2).value.toInt.arcsec,
      (diameter.toUnit[ArcSecond] * 2).value.toInt.arcsec
    )

  /**
    * F2 area where the probe arm can reach centered with a given posAngle and offset
    */
  def candidatesAreaAt(l: F2LyotWheel, posAngle: Angle, offsetPos: Offset): ShapeExpression =
    candidatesArea(l) ↗ offsetPos ⟲ posAngle

  /**
    * F2 area reachable by the problem arm for a set of posAngles and offsets
    */
  def candidatesAreaAt(l: F2LyotWheel, posAngles: List[Angle], offsetPositions: List[Offset]): ShapeExpression =
    (for {
      a <- posAngles
      o <- offsetPositions
    } yield candidatesAreaAt(l, a, o)).fold(ShapeExpression.Empty)(_ ∩ _)

object candidatesArea extends F2CandidatesArea
