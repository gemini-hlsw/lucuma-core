// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Enumerated

sealed abstract class SpectroscopyCapabilities extends Product with Serializable

object SpectroscopyCapabilities {
  case object NodAndShuffle extends SpectroscopyCapabilities
  case object Polarimetry   extends SpectroscopyCapabilities
  case object Coronagraphy  extends SpectroscopyCapabilities

  implicit val SpectroscopyCapabilitiesEnumerated: Enumerated[SpectroscopyCapabilities] =
    Enumerated.of(NodAndShuffle, Polarimetry, Coronagraphy)
}
