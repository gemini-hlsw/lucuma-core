// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.*
import lucuma.core.util.arb.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.*

trait ArbUser {
  import ArbGid.given
  import ArbRole.given
  import ArbOrcidProfile.given

  given Arbitrary[GuestUser] =
    Arbitrary(arbitrary[User.Id].map(GuestUser(_)))

  given Cogen[GuestUser] =
    Cogen[User.Id].contramap(_.id)

  given Arbitrary[ServiceUser] =
    Arbitrary {
      for {
        id <- arbitrary[User.Id]
        n  <- arbitrary[String]
      } yield ServiceUser(id, n)
    }

  given Cogen[ServiceUser] =
    Cogen[(User.Id, String)].contramap(x => (x.id, x.name))

  given Arbitrary[StandardUser] =
    Arbitrary {
      for {
        id <- arbitrary[User.Id]
        r  <- arbitrary[StandardRole]
        o  <- arbitrary[List[StandardRole]]
        p  <- arbitrary[OrcidProfile]
      } yield StandardUser(id, r, o, p)
    }

  given Cogen[StandardUser] =
    Cogen[(User.Id, StandardRole, List[StandardRole], OrcidProfile)].contramap(x =>
      (x.id, x.role, x.otherRoles, x.profile)
    )

  given Arbitrary[User] =
    Arbitrary {
      for {
        guest    <- arbitrary[GuestUser]
        service  <- arbitrary[ServiceUser]
        standard <- arbitrary[StandardUser]
        user     <- Gen.oneOf(guest, service, standard)
      } yield user
    }

  given Cogen[User] =
    Cogen[Either[GuestUser, Either[ServiceUser, StandardUser]]].contramap {
      case guest: GuestUser       => Left(guest)
      case service: ServiceUser   => Right(Left(service))
      case standard: StandardUser => Right(Right(standard))
    }

}

object ArbUser extends ArbUser
