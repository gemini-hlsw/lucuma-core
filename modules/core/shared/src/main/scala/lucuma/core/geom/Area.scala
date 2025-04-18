// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.Order
import monocle.Prism

/**
 * Shape area is useful for comparison, for example to determine which area
 * resulting from the intersection of a guide probe arm with the science FOV
 * is smaller.
 *
 * @param toMicroarcsecondsSquared area in µas^2^
 */
opaque type Area = Long

object Area {
  /** @group Constants */
  lazy val MinArea: Area = 0L

  /** @group Constants */
  lazy val MaxArea: Area = Long.MaxValue

  extension (a: Area)
    inline def toMicroarcsecondsSquared: Long = a


  /**
   * Prism from Long in µas^2^ into Area and back.
   * @group Optics
   */
  val fromMicroarcsecondsSquared: Prism[Long, Area] =
    Prism((n: Long) => Some(n).filter(_ >= 0))(_.toMicroarcsecondsSquared)

  /**
   * Sorts Area by size, ascending. This may be used, for example, to select a
   * a guide star that results in the minimum vignetting of a science area by a
   * probe arm.
   *
   * @group Typeclass Instances
   */
  given (using o: Order[Long]): Order[Area] =  o

}
