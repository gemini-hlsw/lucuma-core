// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.UserProfile
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbOrcidProfile:
  import ArbOrcidId.given
  import ArbUserProfile.given

  given Arbitrary[OrcidProfile] =
    Arbitrary:
      for
        id      <- arbitrary[OrcidId]
        profile <- arbitrary[UserProfile]
      yield OrcidProfile(id, profile)

  given Cogen[OrcidProfile] =
    Cogen[(OrcidId, UserProfile)].contramap: a =>
      (a.orcidId, a.profile)

object ArbOrcidProfile extends ArbOrcidProfile
