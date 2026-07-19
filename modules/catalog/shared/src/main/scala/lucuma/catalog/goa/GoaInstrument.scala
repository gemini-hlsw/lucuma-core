// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import lucuma.core.enums.Instrument

trait GoaInstrumentOps:

  extension (instrument: Instrument)
    def goaName: Option[String] =
      instrument match
        case Instrument.GmosNorth  => Some("GMOS-N")
        case Instrument.GmosSouth  => Some("GMOS-S")
        case Instrument.Flamingos2 => Some("F2")
        case Instrument.Gnirs      => Some("GNIRS")
        case Instrument.Niri       => Some("NIRI")
        case Instrument.Ghost      => Some("GHOST")
        case Instrument.Gpi        => Some("GPI")
        case Instrument.Gsaoi      => Some("GSAOI")
        case Instrument.Alopeke    => Some("ALOPEKE")
        case Instrument.Zorro      => Some("ZORRO")
        case Instrument.Igrins2    => Some("IGRINS2")
        case _                     => None

object syntax extends GoaInstrumentOps
