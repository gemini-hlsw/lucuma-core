// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import lucuma.core.model.EphemerisKey

class HorizonsClientCometSuite extends HorizonsClientSearchSuite(HorizonsClient.Search.Comet.apply):
  testEmptyResults("oideyqiduye")
  testMultipleResults(
    "hu",
    (EphemerisKey.Comet("67P"), "Churyumov-Gerasimenko"),
    (EphemerisKey.Comet("106P"), "Schuster"),
    (EphemerisKey.Comet("130P"), "McNaught-Hughes"),
    (EphemerisKey.Comet("178P"), "Hug-Bell"),
    (EphemerisKey.Comet("C/1880 Y1"), "Pechule"),
  )
  testSingleResult("(Format 1) Hubble (C/1937 P1)")(
    "hubble",
    (EphemerisKey.Comet("C/1937 P1"), "Hubble")
  )
  testSingleResult("(Format 2) 81P/Wild 2 pattern")(
    "81P",
    (EphemerisKey.Comet("81P"), "Wild 2")
  )