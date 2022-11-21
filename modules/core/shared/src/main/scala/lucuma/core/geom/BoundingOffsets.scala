// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * Defines a pair of offsets that surround a shape
 *
 * {{{
 * topLeft
 * +------------+
 * |            |
 * |            |              ^
 * |            |              |   q
 * |            |              |
 * |            |              |
 * +------------+         <----+
 *          bottomRight     p
 * }}}
 */
final case class BoundingOffsets(topLeft: Offset, bottomRight: Offset) {

  /**
   * The vertices (p1, p2, p3, p4) are in the order indicated below:
   *
   * {{{
   * p1 p2
   * |---|
   * |   |
   * |---|
   * p4 p3
   * }}}
   */
  def vertices: (Offset, Offset, Offset, Offset) =
    (topLeft, Offset(bottomRight.p, topLeft.q), bottomRight, Offset(topLeft.p, bottomRight.q))

  /**
   * Widest side expressed as an angle, Useful to do queries covering a shape
   */
  def maxSide: Angle = {
    val p = topLeft.p.toAngle.difference(bottomRight.p.toAngle)
    val q = topLeft.q.toAngle.difference(bottomRight.q.toAngle)
    Angle.AngleOrder.max(p, q)
  }
}
