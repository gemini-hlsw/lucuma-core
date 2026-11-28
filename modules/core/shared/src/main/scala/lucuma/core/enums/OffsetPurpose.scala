// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.syntax.shapeexpression.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.util.Enumerated

/**
 * What a telescope position is for: observing the object being measured, or the
 * sky background used to subtract from it.  Only object positions count toward
 * the exposures an observation requests.
 *
 * For an observation whose object is the sky itself (e.g., a sky calibration)
 * the positions are `Object`.
 */
enum OffsetPurpose(val tag: String, val name: String) derives Enumerated:
  case Object extends OffsetPurpose("object", "Object")
  case Sky    extends OffsetPurpose("sky",    "Sky")

  /**
   * The guide state a position with this purpose takes unless specified:
   * object positions are guided, sky positions are not.
   */
  def defaultGuiding: StepGuideState =
    this match
      case Object => StepGuideState.Enabled
      case Sky    => StepGuideState.Disabled

object OffsetPurpose:

  /**
   * The purpose a position has by geometry alone: `Object` when the target (at
   * the base position) falls inside `scienceArea` with the telescope at
   * `offset`, `Sky` otherwise.  A target on the edge of the science area counts
   * as inside.
   *
   * The science area is taken at position angle zero.  Offsets are expressed in
   * the instrument (p, q) frame, which rotates with the position angle along
   * with the science area, so the result does not depend on it.
   */
  def fromScienceArea(scienceArea: ShapeExpression)(offset: Offset)(using ShapeInterpreter): OffsetPurpose =
    // `intersects` rather than `contains`, so that the boundary is included.
    val base = ShapeExpression.point(Offset.Zero).eval
    if scienceArea.shapeAt(offset, Angle.Angle0).eval.intersects(base) then Object
    else Sky
