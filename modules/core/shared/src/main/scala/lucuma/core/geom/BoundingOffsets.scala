// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.Order
import cats.syntax.order.*
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

  private given Order[Angle] = Angle.SignedAngleOrder

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

  /**
   * Fast containment check
   */
  def contains(o: Offset): Boolean =
    o.p.toAngle >= bottomRight.p.toAngle &&
    o.p.toAngle <= topLeft.p.toAngle &&
    o.q.toAngle >= bottomRight.q.toAngle &&
    o.q.toAngle <= topLeft.q.toAngle

  /**
   * Whether the boxes share at least a point. Disjoint boxes mean the shapes they bound cannot
   * intersect, so this is a cheap pre-check before an overlay.
   */
  def intersects(that: BoundingOffsets): Boolean =
    bottomRight.p.toAngle <= that.topLeft.p.toAngle &&
    that.bottomRight.p.toAngle <= topLeft.p.toAngle &&
    bottomRight.q.toAngle <= that.topLeft.q.toAngle &&
    that.bottomRight.q.toAngle <= topLeft.q.toAngle
}
