// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import lucuma.core.math.Angle
import lucuma.core.optics.SplitMono

/**
 * Gaussian source. For a good discussion of seeing see the ["Astronomical seeing" wikipedia
 * entry](https://en.wikipedia.org/wiki/Astronomical_seeing).
 *
 * @param fwhm
 *   full width at half maximum of the seeing disc (typically in arcsec)
 */
final case class GaussianSource(fwhm: Angle)

trait GaussianSourceOptics {

  /**
   * Conversion between GaussianSource and arcsec of the FWHM of the seeing disc.
   *
   * @group Optics
   */
  val arcsec: SplitMono[GaussianSource, BigDecimal] =
    SplitMono(
      g => new java.math.BigDecimal(g.fwhm.toMicroarcseconds).movePointLeft(6),
      d => GaussianSource(Angle.fromMicroarcseconds(d.underlying.movePointRight(6).longValue))
    )

  implicit val EqGaussiansource: Eq[GaussianSource] = Eq.by(_.fwhm)
}

object GaussianSource extends GaussianSourceOptics {
  val Zero: GaussianSource = GaussianSource(Angle.Angle0)
}
