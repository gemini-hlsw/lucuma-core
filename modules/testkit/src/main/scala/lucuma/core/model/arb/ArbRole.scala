// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.enums.Partner
import lucuma.core.model.GuestRole
import lucuma.core.model.Role
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole
import lucuma.core.util.arb.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbRole {
  import ArbGid.given
  import ArbEnumerated.given

  given Arbitrary[ServiceRole] =
    Arbitrary(arbitrary[String].map(ServiceRole(_)))

  given  Cogen[ServiceRole] =
    Cogen[String].contramap(_.serviceName)

  given Arbitrary[StandardRole.Pi] =
    Arbitrary(arbitrary[StandardRole.Id].map(StandardRole.Pi(_)))

  given Cogen[StandardRole.Pi] =
    Cogen[StandardRole.Id].contramap(_.id)

  given Arbitrary[StandardRole.Ngo] =
    Arbitrary {
      for {
        id <- arbitrary[StandardRole.Id]
        p  <- arbitrary[Partner]
      } yield StandardRole.Ngo(id, p)
    }

  given Cogen[StandardRole.Ngo] =
    Cogen[(StandardRole.Id, Partner)].contramap(x => (x.id, x.partner))

  given Arbitrary[StandardRole.Staff] =
    Arbitrary(arbitrary[StandardRole.Id].map(StandardRole.Staff(_)))

  given Cogen[StandardRole.Staff] =
    Cogen[StandardRole.Id].contramap(_.id)

  given Arbitrary[StandardRole.Admin] =
    Arbitrary(arbitrary[StandardRole.Id].map(StandardRole.Admin(_)))

  given Cogen[StandardRole.Admin] =
    Cogen[StandardRole.Id].contramap(_.id)

  given Arbitrary[StandardRole] =
    Arbitrary {
      for {
        pi    <- arbitrary[StandardRole.Pi]
        ngo   <- arbitrary[StandardRole.Ngo]
        staff <- arbitrary[StandardRole.Staff]
        admin <- arbitrary[StandardRole.Admin]
        role  <- Gen.oneOf(pi, ngo, staff, admin)
      } yield role
    }

  given Cogen[StandardRole] =
    Cogen[Either[
      StandardRole.Pi,
      Either[StandardRole.Ngo, Either[StandardRole.Staff, StandardRole.Admin]]
    ]].contramap {
      case pi: StandardRole.Pi       => Left(pi)
      case ngo: StandardRole.Ngo     => Right(Left(ngo))
      case staff: StandardRole.Staff => Right(Right(Left(staff)))
      case admin: StandardRole.Admin => Right(Right(Right(admin)))
    }

  given Arbitrary[Role] =
    Arbitrary {
      for {
        guest    <- Gen.const(GuestRole)
        service  <- arbitrary[ServiceRole]
        standard <- arbitrary[StandardRole]
        role     <- Gen.oneOf(guest, service, standard)
      } yield role
    }

  given Cogen[Role] =
    Cogen[Option[Either[ServiceRole, StandardRole]]].contramap {
      case GuestRole              => None
      case service: ServiceRole   => Some(Left(service))
      case standard: StandardRole => Some(Right(standard))
    }

}

object ArbRole extends ArbRole
