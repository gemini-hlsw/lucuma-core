// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnit._

import scala.collection.immutable.SortedMap

sealed trait BrightnessProfile extends Product with Serializable {
  def bands: List[Band]
}

final case class PointBrightnessProfile(
  brightnesses: SortedMap[Band, TargetBrightness[Brightness[Integrated]]],
  sed:          SpectralDistribution[Brightness[Integrated]]
) extends BrightnessProfile {
  lazy val bands: List[Band] = brightnesses.keys.toList
}

final case class UniformBrightnessProfile(
  brightnesses: SortedMap[Band, TargetBrightness[Brightness[Surface]]],
  sed:          SpectralDistribution[Brightness[Surface]]
) extends BrightnessProfile {
  lazy val bands: List[Band] = brightnesses.keys.toList
}

/**
 * Gaussian source. For a good discussion of seeing see the ["Astronomical seeing" wikipedia
 * entry](https://en.wikipedia.org/wiki/Astronomical_seeing).
 *
 * @param fwhm
 *   full width at half maximum of the seeing disc (typically in arcsec)
 */
final case class GaussianBrightnessProfile(
  source:       GaussianSource,
  brightnesses: SortedMap[Band, TargetBrightness[Brightness[Integrated]]],
  sed:          SpectralDistribution[Brightness[Integrated]]
) extends BrightnessProfile {
  lazy val bands: List[Band] = brightnesses.keys.toList
}

object BrightnessProfile {
  // TODO This right
  implicit val eqBrightnessProfile: Eq[BrightnessProfile] = Eq.fromUniversalEquals

  // TODO Lenses
}
