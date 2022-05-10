// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class PlanetaryNebulaSpectrum(
  val tag:  String,
  val name: String
) extends Product
    with Serializable

object PlanetaryNebulaSpectrum {
  case object NGC7009 extends PlanetaryNebulaSpectrum("NGC7009", "NGC7009 (100nm - 1.1μm)")
  case object IC5117  extends PlanetaryNebulaSpectrum("IC5117", "IC5117 (480nm - 2.5μm)")

  implicit val enumPlanetaryNebulaSpectrum: Enumerated[PlanetaryNebulaSpectrum] =
    Enumerated.from(NGC7009, IC5117).withTag(_.tag)

  implicit val displayPlanetaryNebulaSpectrum: Display[PlanetaryNebulaSpectrum] =
    Display.byShortName(_.name)
}
