// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.syntax

import gsp.math.Angle

final class IntOps(val self: Int) extends AnyVal {

  private def scaled(µas: Long): Angle =
    Angle.signedMicroarcseconds.reverseGet(self.toLong * µas)

  def degrees: Angle =
    scaled(1000L * 1000L * 60L * 60L)

  def deg: Angle =
    degrees

  def arcminutes: Angle =
    scaled(1000L * 1000L * 60L)

  def arcmin: Angle =
    arcminutes

  def am: Angle =
    arcminutes

  def arcseconds: Angle =
    scaled(1000L * 1000L)

  def arcsec: Angle =
    arcseconds

  def as: Angle =
    arcseconds

  def milliarcseconds: Angle =
    scaled(1000L)

  def mas: Angle =
    milliarcseconds

  def microarcseconds: Angle =
    scaled(1)

  def µas: Angle =
    microarcseconds

}

trait ToIntOps {
  implicit def ToIntOps(i: Int): IntOps = new IntOps(i)
}

object int extends ToIntOps
