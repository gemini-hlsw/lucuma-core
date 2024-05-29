// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbOrcidProfile {
  import ArbOrcidId.given

  given Arbitrary[OrcidProfile] =
    Arbitrary {
      for {
        id           <- arbitrary[OrcidId]
        givenName    <- arbitrary[Option[String]]
        familyName   <- arbitrary[Option[String]]
        creditName   <- arbitrary[Option[String]]
        primaryEmail <- arbitrary[Option[String]]
      } yield OrcidProfile(id, givenName, familyName, creditName, primaryEmail)
    }

  given Cogen[OrcidProfile] =
    Cogen[(OrcidId, Option[String], Option[String], Option[String], Option[String])].contramap(x =>
      (x.orcidId, x.givenName, x.familyName, x.creditName, x.primaryEmail)
    )

}

object ArbOrcidProfile extends ArbOrcidProfile
