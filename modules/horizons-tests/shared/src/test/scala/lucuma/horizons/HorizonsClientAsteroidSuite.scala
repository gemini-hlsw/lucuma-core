// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import lucuma.core.model.EphemerisKey

class HorizonsClientAsteroidSuite extends HorizonsClientSearchSuite(HorizonsClient.Search.Asteroid.apply):
  testEmptyResults("oideyqiduye")
  testMultipleResults(
    "her",
    (EphemerisKey.AsteroidNew("A868 RA"), "Hera"),
    (EphemerisKey.AsteroidNew("A872 JA"), "Hermione"),
    (EphemerisKey.AsteroidNew("A874 DA"), "Hertha"),
    (EphemerisKey.AsteroidNew("A879 TC"), "Hersilia"),
    (EphemerisKey.AsteroidNew("A880 DB"), "Aschera"),
  )
  testSingleResult("(Format 1) 90377 Sedna (2003 VB12)")(
    "sedna",
    (EphemerisKey.AsteroidNew("2003 VB12"), "Sedna"),
  )
  testSingleResult("(Format 2) 29 Amphitrite")(
    "amphitrite",
    (EphemerisKey.AsteroidNew("A854 EB"), "Amphitrite"),
  )
  testSingleResult("(Format 3) (2016 GB222)")(
    "2016 GB222",
    (EphemerisKey.AsteroidNew("2016 GB222"), "2016 GB222"),
  )
  testSingleResult("(Format 4) 418993 (2009 MS9)")(
    "2009 MS9",
    (EphemerisKey.AsteroidNew("2009 MS9"), "2009 MS9"),
  )
  testSingleResult("(Format 5) 1I/'Oumuamua (A/2017 U1)")(
    "A/2017 U1",
    (EphemerisKey.AsteroidNew("A/2017 U1"), "A/2017 U1"),
  )