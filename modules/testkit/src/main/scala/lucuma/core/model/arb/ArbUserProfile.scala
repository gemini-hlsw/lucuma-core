// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.UserProfile
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbUserProfile:

  given Arbitrary[UserProfile] =
    Arbitrary:
      for
        givenName    <- arbitrary[Option[String]]
        familyName   <- arbitrary[Option[String]]
        creditName   <- arbitrary[Option[String]]
        primaryEmail <- arbitrary[Option[String]]
      yield UserProfile(givenName, familyName, creditName, primaryEmail)

  given Cogen[UserProfile] =
    Cogen[(Option[String], Option[String], Option[String], Option[String])].contramap: x =>
      (x.givenName, x.familyName, x.creditName, x.email)

object ArbUserProfile extends ArbUserProfile