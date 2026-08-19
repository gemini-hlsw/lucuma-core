// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import monocle.Focus
import monocle.Lens

/**
 * The size of a focal-plane aperture along the two telescope-offset axes,
 * centered on the aperture's own position.
 *
 * `p` and `q` are full extents, not half-extents: a 0.5" x 330" GMOS long slit
 * is `ApertureExtent(0.5", 330")`. Modelling both axes (rather than a slit
 * width alone) is what lets IFUs participate — an IFU is a field, and a p
 * offset well beyond any slit width can still land on target.
 */
final case class ApertureExtent(p: Angle, q: Angle):

  /**
   * True if `offset` falls strictly inside the aperture. Strict on both axes,
   * so a target exactly on the edge does not count as contained.
   */
  def contains(offset: Offset): Boolean =
    ApertureExtent.within(offset.p.toAngle, p) && ApertureExtent.within(offset.q.toAngle, q)

  /** This extent with its axes exchanged. */
  def swap: ApertureExtent =
    ApertureExtent(q, p)

object ApertureExtent:

  /** An aperture of equal extent on both axes. */
  def square(side: Angle): ApertureExtent =
    ApertureExtent(side, side)

  // Compares against the half extent without halving it, so odd microarcsecond
  // extents don't lose a µas to truncation.
  private def within(a: Angle, extent: Angle): Boolean =
    Math.abs(Angle.signedMicroarcseconds.get(a)) * 2L < extent.toMicroarcseconds

  // Ordered by magnitude; an extent is a size, never a signed angle.
  /** @group Typeclass Instances */
  given Order[ApertureExtent] =
    Order.by(a => (a.p.toMicroarcseconds, a.q.toMicroarcseconds))

  /** @group Typeclass Instances */
  given Show[ApertureExtent] =
    Show.fromToString

  /** @group Optics */
  val p: Lens[ApertureExtent, Angle] =
    Focus[ApertureExtent](_.p)

  /** @group Optics */
  val q: Lens[ApertureExtent, Angle] =
    Focus[ApertureExtent](_.q)
