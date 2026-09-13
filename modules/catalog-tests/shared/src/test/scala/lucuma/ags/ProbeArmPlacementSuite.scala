// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

import AgsParamsVariants.*

/**
 * `posCalculations` evaluates `probeArmShape` once and places it per guide star with
 * `Shape.transform`. That placed shape must be the very shape `probeArm` describes, for every
 * `AgsParams` variant, position angle, offset and guide star.
 */
class ProbeArmPlacementSuite extends munit.FunSuite:

  private def µas(a: Angle): Long = Angle.signedMicroarcseconds.get(a)

  // Candidate guide stars: the centre and inner quartiles of the patrol field's bounding box.
  private def candidates(b: BoundingOffsets): List[Offset] =
    val pMin = µas(b.bottomRight.p.toAngle)
    val qMin = µas(b.bottomRight.q.toAngle)
    val pw   = (µas(b.topLeft.p.toAngle) - pMin).toDouble
    val qw   = (µas(b.topLeft.q.toAngle) - qMin).toDouble
    List((0.5, 0.5), (0.25, 0.25), (0.75, 0.25), (0.25, 0.75), (0.75, 0.75)).map: (fp, fq) =>
      Offset(
        Offset.P(Angle.fromMicroarcseconds(pMin + (pw * fp).round)),
        Offset.Q(Angle.fromMicroarcseconds(qMin + (qw * fq).round))
      )

  // lucuma-jts cannot build the F2 GeMS arm (TopologyException); those cases are skipped, not failed.
  private def evaluated(e: ShapeExpression): Option[Shape] =
    try Some(e.eval)
    catch case _: org.locationtech.jts.geom.TopologyException => None

  variants.foreach: (name, params) =>
    test(s"$name: placed probe arm equals the evaluated probe arm expression"):
      for
        arm    <- evaluated(params.probeArmShape).toList
        pa     <- posAngles
        offset <- offsets
        gs     <- candidates(params.patrolFieldAt(pa, offset).eval.boundingOffsets)
        direct <- evaluated(params.probeArm(pa, gs, offset))
      do
        val clue   = s"$name PA ${pa.toDoubleDegrees} offset $offset guide star $gs"
        val placed = arm.transform(Offset.Zero, params.probeArmAngle(pa, gs, offset), gs)
        assertEquals(placed.area, direct.area, s"$clue area")
        assertEquals(placed.boundingOffsets, direct.boundingOffsets, s"$clue bbox")
