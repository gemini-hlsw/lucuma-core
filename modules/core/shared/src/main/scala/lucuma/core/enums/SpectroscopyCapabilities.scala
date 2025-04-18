// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class SpectroscopyCapabilities(val tag: String) extends Product with Serializable

object SpectroscopyCapabilities {
  case object NodAndShuffle extends SpectroscopyCapabilities("nod_and_shuffle")
  case object Polarimetry   extends SpectroscopyCapabilities("polarimetry")
  case object Coronagraphy  extends SpectroscopyCapabilities("coronagraphy")

  implicit val SpectroscopyCapabilitiesEnumerated: Enumerated[SpectroscopyCapabilities] =
    Enumerated.from(NodAndShuffle, Polarimetry, Coronagraphy).withTag(_.tag)
}
