// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.ghost

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.geom.ghost.scienceArea.fov
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

trait GhostIfuPatrolField:

  // 1 mm in arcsec at GHOST plate scale (1/0.61 arcsec/mm)
  val IFUSeparation: Angle = Angle.fromDoubleArcseconds(1.0 / 0.61)

  // 2 mm separation used for HR sky fiber and IFU boundary offsets
  val HRFiberOffset: Angle = Angle.fromDoubleArcseconds(2.0 / 0.61)

  // IFU1 patrols the West half of FOV (negative P side)
  val ifu1PatrolField: ShapeExpression =
    ShapeExpression.rectangleAt(
      ((-222).arcsec.p, 222.arcsec.q),
      (HRFiberOffset.p, (-222).arcsec.q)
    ) ∩ fov

  // IFU2 patrols the East half of FOV (positive P side)
  val ifu2PatrolField: ShapeExpression =
    ShapeExpression.rectangleAt(
      ((-HRFiberOffset).p, 222.arcsec.q),
      (222.arcsec.p, (-222).arcsec.q)
    ) ∩ fov

  def ifu1PatrolFieldAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    ifu1PatrolField.shapeAt(offsetPos, posAngle)

  def ifu2PatrolFieldAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    ifu2PatrolField.shapeAt(offsetPos, posAngle)

  def ifu1PatrolFieldAt(posAngles: List[Angle], offsets: List[Offset]): ShapeExpression =
    ifu1PatrolField.intersectionShape(posAngles, offsets)

  def ifu2PatrolFieldAt(posAngles: List[Angle], offsets: List[Offset]): ShapeExpression =
    ifu2PatrolField.intersectionShape(posAngles, offsets)

object GhostIfuPatrolField extends GhostIfuPatrolField
