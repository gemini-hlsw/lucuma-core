// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import lucuma.core.model.Ephemeris

class HorizonsClientCometSuite extends HorizonsClientSearchSuite(HorizonsClient.Search.Comet.apply):
  testEmptyResults("oideyqiduye")
  testMultipleResults(
    "hu",
    (Ephemeris.Key.Comet("67P"), "Churyumov-Gerasimenko"),
    (Ephemeris.Key.Comet("106P"), "Schuster"),
    (Ephemeris.Key.Comet("130P"), "McNaught-Hughes"),
    (Ephemeris.Key.Comet("178P"), "Hug-Bell"),
    (Ephemeris.Key.Comet("C/1880 Y1"), "Pechule"),
  )
  testSingleResult("(Format 1) Hubble (C/1937 P1)")(
    "hubble",
    (Ephemeris.Key.Comet("C/1937 P1"), "Hubble")
  )
  testSingleResult("(Format 2) 81P/Wild 2 pattern")(
    "81P",
    (Ephemeris.Key.Comet("81P"), "Wild 2")
  )