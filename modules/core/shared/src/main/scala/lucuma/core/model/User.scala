// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.ApplicativeError
import cats.implicits._
import eu.timepit.refined.refineMV
import eu.timepit.refined.types.numeric.PosLong
import java.security.AccessControlException
import lucuma.core.util.Gid

/** A user has [at least] an identity and a role. */
sealed trait User extends Product with Serializable {

  def id:   User.Id
  def role: Role

  /**
   * A name to display in interfaces. This is never empty, and respects users' formatting
   * preferences as specified in their ORCID record.
   */
  def displayName: String

  /** Verity that this user has access greater than or equal to `access`. */
  final def verifyAccess[F[_]](access: Access)(
    implicit ev: ApplicativeError[F, Throwable]
  ): F[Unit] =
    ev.raiseError(new AccessControlException(s"$displayName (User ${id.value}, $role) does not have required access $access."))
      .whenA(role.access < access)

}

object User {

  /** Every user has a unique id. */
  case class Id(value: PosLong) {
    override def toString = this.show
  }
  object Id {
    implicit val GidUserGid: Gid[Id] = Gid.instance(refineMV('u'), _.value, apply)
  }

}

/** Guest users have the weakest `Role` and no identifying information. */
final case class GuestUser(id: User.Id) extends User {
  val role = GuestRole
  val displayName = "Guest User"
}

/** Service users have the strongest `Role` and represent services themselves. */
final case class ServiceUser(id: User.Id, name: String) extends User {
  val role = ServiceRole(name)
  val displayName = s"Service User ($name)"
}

/**
 * Standard users are authenticated and have a current role and ORCID profile, as well as a set of
 * other roles they can assume.
 */
final case class StandardUser(
  id:         User.Id,
  role:       StandardRole,
  otherRoles: List[StandardRole],
  profile:    OrcidProfile
) extends User {
  val displayName = profile.displayName
}
