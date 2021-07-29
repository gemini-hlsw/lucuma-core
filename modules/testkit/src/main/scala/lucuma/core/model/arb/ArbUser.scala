// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model._
import lucuma.core.util.arb._
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbUser {
  import ArbGid._
  import ArbEnumerated._
  import ArbRole._
  import ArbOrcidProfile._

  implicit val ArbGuestUser: Arbitrary[GuestUser] =
    Arbitrary(arbitrary[User.Id].map(GuestUser(_)))

  implicit val CogGuestUser: Cogen[GuestUser] =
    Cogen[User.Id].contramap(_.id)

  implicit val ArbServiceUser: Arbitrary[ServiceUser] =
    Arbitrary {
      for {
        id <- arbitrary[User.Id]
        n  <- arbitrary[String]
      } yield ServiceUser(id, n)
    }

  implicit val CogServiceUser: Cogen[ServiceUser] =
    Cogen[(User.Id, String)].contramap(x => (x.id, x.name))

  implicit val ArbStandardUser: Arbitrary[StandardUser] =
    Arbitrary {
      for {
        id <- arbitrary[User.Id]
        r  <- arbitrary[StandardRole]
        o  <- arbitrary[List[StandardRole]]
        p  <- arbitrary[OrcidProfile]
      } yield StandardUser(id, r, o, p)
    }

  implicit val CogStandardUser: Cogen[StandardUser] =
    Cogen[(User.Id, StandardRole, List[StandardRole], OrcidProfile)].contramap(x =>
      (x.id, x.role, x.otherRoles, x.profile)
    )

  implicit val ArbUser: Arbitrary[User] =
    Arbitrary {
      for {
        guest    <- arbitrary[GuestUser]
        service  <- arbitrary[ServiceUser]
        standard <- arbitrary[StandardUser]
        user     <- Gen.oneOf(guest, service, standard)
      } yield user
    }

  implicit val CogUser: Cogen[User] =
    Cogen[Either[GuestUser, Either[ServiceUser, StandardUser]]].contramap {
      case guest: GuestUser       => Left(guest)
      case service: ServiceUser   => Right(Left(service))
      case standard: StandardUser => Right(Right(standard))
    }

}

object ArbUser extends ArbUser
