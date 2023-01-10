// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class HIIRegionSpectrum(
  val tag:  String,
  val name: String
) extends Product
    with Serializable

object HIIRegionSpectrum {
  case object OrionNebula extends HIIRegionSpectrum("OrionNebula", "Orion (100nm - 1.0μm)")

  implicit val enumHIIRegionSpectrum: Enumerated[HIIRegionSpectrum] =
    Enumerated.from[HIIRegionSpectrum](OrionNebula).withTag(_.tag)

  implicit val displayHIIRegionSpectrum: Display[HIIRegionSpectrum] =
    Display.byShortName(_.name)
}
