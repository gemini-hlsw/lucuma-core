// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class GalaxySpectrum(
  val tag:  String,
  val name: String
) extends Product
    with Serializable

object GalaxySpectrum {
  case object Elliptical extends GalaxySpectrum("Elliptical", "Elliptical (22nm - 9.7μm)")
  case object Spiral     extends GalaxySpectrum("Spiral", "Spiral (Sc, 22nm - 9.7μm)")

  implicit val enumGalaxySpectrum: Enumerated[GalaxySpectrum] =
    Enumerated.from(Elliptical, Spiral).withTag(_.tag)

  implicit val displayGalaxySpectrum: Display[GalaxySpectrum] =
    Display.byShortName(_.name)
}
