// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ObservingModeType(val dbTag: String, val instrument: Instrument):
  case GmosNorthLongSlit  extends ObservingModeType("gmos_north_long_slit",  Instrument.GmosNorth)
  case GmosSouthLongSlit  extends ObservingModeType("gmos_south_long_slit",  Instrument.GmosSouth)
  case Flamingos2LongSlit extends ObservingModeType("flamingos_2_long_slit", Instrument.Flamingos2)

object ObservingModeType:

  given Enumerated[ObservingModeType] =
    Enumerated.from(
      GmosNorthLongSlit,
      GmosSouthLongSlit,
      Flamingos2LongSlit
    ).withTag(_.dbTag)
