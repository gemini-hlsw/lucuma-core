// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import lucuma.core.model.EphemerisKey

class HorizonsClientMajorBodySuite extends HorizonsClientSearchSuite(HorizonsClient.Search.MajorBody.apply):
  testEmptyResults("oideyqiduye")
  testEmptyResults("hh", Some("small-body fallthrough (many)"))
  testEmptyResults("hermione", Some("small-body fallthrough (single)"))
  testMultipleResults(
    "mar",
    (EphemerisKey.MajorBody(4), "Mars Barycenter"),
    (EphemerisKey.MajorBody(499), "Mars"),
    (EphemerisKey.MajorBody(723), "Margaret"),
    (EphemerisKey.MajorBody(617), "Patroclus (primary body)"),
    (EphemerisKey.MajorBody(50000), "Quaoar (primary body)")    //  up in October of 2022 (no idea why)
  )
  testSingleResult("trailing space (!)")(
    "charon",
    (EphemerisKey.MajorBody(901), "Charon")
  )
  testSingleResult("no trailing space")(
    "europa",
    (EphemerisKey.MajorBody(502), "Europa")
  )
