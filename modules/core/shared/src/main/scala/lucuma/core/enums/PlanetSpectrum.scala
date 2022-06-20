// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class PlanetSpectrum(
  val tag:  String,
  val name: String
) extends Product
    with Serializable

object PlanetSpectrum {
  case object Mars    extends PlanetSpectrum("Mars", "Mars (540nm - 5.0μm)")
  case object Jupiter extends PlanetSpectrum("Jupiter", "Jupiter (530nm - 5.9μm)")
  case object Saturn  extends PlanetSpectrum("Saturn", "Saturn (530nm - 5.9μm)")
  case object Uranus  extends PlanetSpectrum("Uranus", "Uranus (540nm - 5.0μm)")
  case object Neptune extends PlanetSpectrum("Neptune", "Neptune (540nm - 5.0μm)")

  implicit val enumPlanetSpectrum: Enumerated[PlanetSpectrum] =
    Enumerated.from(Mars, Jupiter, Saturn, Uranus, Neptune).withTag(_.tag)

  implicit val displayPlanetSpectrum: Display[PlanetSpectrum] =
    Display.byShortName(_.name)
}
