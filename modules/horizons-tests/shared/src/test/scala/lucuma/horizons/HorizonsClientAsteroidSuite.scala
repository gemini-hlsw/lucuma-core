// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import lucuma.core.model.Ephemeris

class HorizonsClientAsteroidSuite extends HorizonsClientSearchSuite(HorizonsClient.Search.Asteroid.apply):
  testEmptyResults("oideyqiduye")
  testMultipleResults(
    "her",
    (Ephemeris.Key.AsteroidNew("A868 RA"), "Hera"),
    (Ephemeris.Key.AsteroidNew("A872 JA"), "Hermione"),
    (Ephemeris.Key.AsteroidNew("A874 DA"), "Hertha"),
    (Ephemeris.Key.AsteroidNew("A879 TC"), "Hersilia"),
    (Ephemeris.Key.AsteroidNew("A880 DB"), "Aschera"),
  )
  testSingleResult("(Format 1) 90377 Sedna (2003 VB12)")(
    "sedna",
    (Ephemeris.Key.AsteroidNew("2003 VB12"), "Sedna"),
  )
  testSingleResult("(Format 2) 29 Amphitrite")(
    "amphitrite",
    (Ephemeris.Key.AsteroidNew("A854 EB"), "Amphitrite"),
  )
  testSingleResult("(Format 3) (2016 GB222)")(
    "2016 GB222",
    (Ephemeris.Key.AsteroidNew("2016 GB222"), "2016 GB222"),
  )
  testSingleResult("(Format 4) 418993 (2009 MS9)")(
    "2009 MS9",
    (Ephemeris.Key.AsteroidNew("2009 MS9"), "2009 MS9"),
  )
  testSingleResult("(Format 5) 1I/'Oumuamua (A/2017 U1)")(
    "A/2017 U1",
    (Ephemeris.Key.AsteroidNew("A/2017 U1"), "A/2017 U1"),
  )