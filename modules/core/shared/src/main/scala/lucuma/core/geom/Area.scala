// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.Order
import cats.syntax.eq._
import monocle.Prism

/**
 * Shape area is useful for comparison, for example to determine which area
 * resulting from the intersection of a guide probe arm with the science FOV
 * is smaller.
 *
 * @param toMicroarcsecondsSquared area in µas^2^
 */
sealed class Area protected (val toMicroarcsecondsSquared: Long) {

  // Sanity check ... should be correct via the companion constructor
  assert(toMicroarcsecondsSquared >= 0,
         s"Invariant violated. $toMicroarcsecondsSquared is negative"
  )

  /** String representation of this Area, for debugging purposes only. */
  override final def toString =
    f"Area($toMicroarcsecondsSquared µas^2)"

  /** Areas are equal if their size is equal. */
  override final def equals(a: Any): Boolean =
    a match {
      case a: Area => a.toMicroarcsecondsSquared === toMicroarcsecondsSquared
      case _       => false
    }

  override final def hashCode: Int =
    toMicroarcsecondsSquared.hashCode

}

object Area {

  /** @group Constants */
  lazy val MinArea: Area = new Area(0L) {}

  /** @group Constants */
  lazy val MaxArea: Area = new Area(Long.MaxValue) {}

  /**
   * Prism from Long in µas^2^ into Area and back.
   * @group Optics
   */
  val fromMicroarcsecondsSquared: Prism[Long, Area] =
    Prism((n: Long) => Some(n).filter(_ >= 0).map(new Area(_) {}))(_.toMicroarcsecondsSquared)

  /**
   * Sorts Area by size, ascending. This may be used, for example, to select a
   * a guide star that results in the minimum vignetting of a science area by a
   * probe arm.
   *
   * @group Typeclass Instances
   */
  implicit val AreaOrder: Order[Area]               =
    Order.by(_.toMicroarcsecondsSquared)

}
