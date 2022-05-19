// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.GuestRole
import lucuma.core.model.Partner
import lucuma.core.model.Role
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole
import lucuma.core.util.arb._
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbRole {
  import ArbGid._
  import ArbEnumerated._

  implicit val ArbServiceRole: Arbitrary[ServiceRole] =
    Arbitrary(arbitrary[String].map(ServiceRole(_)))

  implicit val CogServiceRole: Cogen[ServiceRole] =
    Cogen[String].contramap(_.serviceName)

  implicit val ArbStandardRolePi: Arbitrary[StandardRole.Pi] =
    Arbitrary(arbitrary[StandardRole.Id].map(StandardRole.Pi(_)))

  implicit val CogStandardRolePi: Cogen[StandardRole.Pi] =
    Cogen[StandardRole.Id].contramap(_.id)

  implicit val ArbStandardRoleNgo: Arbitrary[StandardRole.Ngo] =
    Arbitrary {
      for {
        id <- arbitrary[StandardRole.Id]
        p  <- arbitrary[Partner]
      } yield StandardRole.Ngo(id, p)
    }

  implicit val CogStandardRoleNgo: Cogen[StandardRole.Ngo] =
    Cogen[(StandardRole.Id, Partner)].contramap(x => (x.id, x.partner))

  implicit val ArbStandardRoleStaff: Arbitrary[StandardRole.Staff] =
    Arbitrary(arbitrary[StandardRole.Id].map(StandardRole.Staff(_)))

  implicit val CogStandardRoleStaff: Cogen[StandardRole.Staff] =
    Cogen[StandardRole.Id].contramap(_.id)

  implicit val ArbStandardRoleAdmin: Arbitrary[StandardRole.Admin] =
    Arbitrary(arbitrary[StandardRole.Id].map(StandardRole.Admin(_)))

  implicit val CogStandardRoleAdmin: Cogen[StandardRole.Admin] =
    Cogen[StandardRole.Id].contramap(_.id)

  implicit val ArbStandardRole: Arbitrary[StandardRole] =
    Arbitrary {
      for {
        pi    <- arbitrary[StandardRole.Pi]
        ngo   <- arbitrary[StandardRole.Ngo]
        staff <- arbitrary[StandardRole.Staff]
        admin <- arbitrary[StandardRole.Admin]
        role  <- Gen.oneOf(pi, ngo, staff, admin)
      } yield role
    }

  implicit val CogStandardRole: Cogen[StandardRole] =
    Cogen[Either[
      StandardRole.Pi,
      Either[StandardRole.Ngo, Either[StandardRole.Staff, StandardRole.Admin]]
    ]].contramap {
      case pi: StandardRole.Pi       => Left(pi)
      case ngo: StandardRole.Ngo     => Right(Left(ngo))
      case staff: StandardRole.Staff => Right(Right(Left(staff)))
      case admin: StandardRole.Admin => Right(Right(Right(admin)))
    }

  implicit val ArbRole: Arbitrary[Role] =
    Arbitrary {
      for {
        guest    <- Gen.const(GuestRole)
        service  <- arbitrary[ServiceRole]
        standard <- arbitrary[StandardRole]
        role     <- Gen.oneOf(guest, service, standard)
      } yield role
    }

  implicit val CogRole: Cogen[Role] =
    Cogen[Option[Either[ServiceRole, StandardRole]]].contramap {
      case GuestRole              => None
      case service: ServiceRole   => Some(Left(service))
      case standard: StandardRole => Some(Right(standard))
    }

}

object ArbRole extends ArbRole
