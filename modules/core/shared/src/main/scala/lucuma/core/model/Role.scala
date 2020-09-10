// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.implicits._
import eu.timepit.refined.refineMV
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.util.Gid

/** Each `Role` has [at least] an `Access`. */
sealed abstract class Role(val access: Access, elaboration: Option[String] = None) {
  final def name = elaboration.foldLeft(access.name)((n, e) => s"$n ($e)")
}

// Special roles
final case object GuestRole extends Role(Access.Guest)
final case class  ServiceRole(serviceName: String) extends Role(Access.Service, Some(serviceName))

// Standard roles
sealed abstract class StandardRole(access: Access, elaboration: Option[String] = None) extends Role(access, elaboration) {
  def id: StandardRole.Id
}
object StandardRole {

  final case class Pi(id: StandardRole.Id) extends StandardRole(Access.Pi)
  final case class Ngo(id: StandardRole.Id, partner: Partner) extends StandardRole(Access.Ngo, Some(partner.name))
  final case class Staff(id: StandardRole.Id) extends StandardRole(Access.Staff)
  final case class Admin(id: StandardRole.Id) extends StandardRole(Access.Admin)

  case class Id(value: PosLong) {
    override def toString = this.show
  }
  object Id {
    implicit val GidId: Gid[Id] = Gid.instance(refineMV('r'), _.value, apply)
  }

}
