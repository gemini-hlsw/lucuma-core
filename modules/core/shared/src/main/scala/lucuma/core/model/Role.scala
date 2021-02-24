// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.implicits._
import eu.timepit.refined.auto._

/**
  * Each user has a current `Role` and a set of other roles they may assume. A role has (at least)
  * an `Access` level.
  */
sealed abstract class Role(val access: Access, elaboration: Option[String] = None) {
  final def name = elaboration.foldLeft(access.name)((n, e) => s"$n ($e)")
}

object Role {
  implicit val eqRole: Eq[Role] = Eq.instance {
    case (GuestRole, GuestRole)             => true
    case (a: ServiceRole, b: ServiceRole)   => a === b
    case (a: StandardRole, b: StandardRole) => a === b
    case _                                  => false
  }
}

// Special roles

/** `GuestRole` allows limited access to temporary programs. */
final case object GuestRole extends Role(Access.Guest)

/** `ServiceRole` is used only for inter-service communication. */
final case class ServiceRole(serviceName: String) extends Role(Access.Service, Some(serviceName))

object ServiceRole {
  implicit val eqServiceRole: Eq[ServiceRole] = Eq.by(_.serviceName)
}

/** The class of roles taken on by authenticated users. */
sealed abstract class StandardRole(access: Access, elaboration: Option[String] = None)
    extends Role(access, elaboration) {
  def id: StandardRole.Id
}
object StandardRole extends WithId {
  protected val idTag = 'r'

  /** The `Pi` role gives access to programs on which the user is a collaborator. */
  final case class Pi(id: StandardRole.Id) extends StandardRole(Access.Pi)

  /** The `Ngo` role is associated with a `Partner` and gives access to programs with affiliated users. */
  final case class Ngo(id: StandardRole.Id, partner: Partner)
      extends StandardRole(Access.Ngo, Some(partner.name))

  /** The `Staff` role gives access to all programs, as well as telescope facilities. */
  final case class Staff(id: StandardRole.Id) extends StandardRole(Access.Staff)

  /** The `Admin` role is a superuser role that allows access to all functionality. */
  final case class Admin(id: StandardRole.Id) extends StandardRole(Access.Admin)

  implicit val eqStandardRole: Eq[StandardRole] = Eq.instance {
    case (Pi(a), Pi(b))         => a === b
    case (Ngo(a, b), Ngo(c, d)) => a === c && b === d
    case (Staff(a), Staff(b))   => a === b
    case (Admin(a), Admin(b))   => a === b
    case _                      => false
  }
}
