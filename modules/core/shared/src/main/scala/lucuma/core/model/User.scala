// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.ApplicativeError
import cats.Eq
import cats.implicits._
import eu.timepit.refined.auto._
import lucuma.core.math.refined._
import lucuma.core.util.WithGid

/** A user has [at least] an identity and a role. */
sealed trait User extends Product with Serializable {

  def id: User.Id
  def role: Role

  /**
   * A name to display in interfaces. This is never empty, and respects users' formatting
   * preferences as specified in their ORCID record.
   */
  def displayName: String

  /** Verity that this user has access greater than or equal to `access`. */
  final def verifyAccess[F[_]](
    access:      Access
  )(implicit ev: ApplicativeError[F, Throwable]): F[Unit] =
    ev.raiseError(
      AccessControlException(displayName, id, role, access)
    ).whenA(role.access < access)

}

object User extends WithGid('u'.refined) {
  implicit val eqUser: Eq[User] = Eq.instance {
    case (a: GuestUser, b: GuestUser)       => a === b
    case (a: ServiceUser, b: ServiceUser)   => a === b
    case (a: StandardUser, b: StandardUser) => a === b
    case _                                  => false
  }

}

/** Guest users have the weakest `Role` and no identifying information. */
final case class GuestUser(id: User.Id) extends User {
  val role        = GuestRole
  val displayName = "Guest User"
}

object GuestUser {
  implicit val eqGuestUser: Eq[GuestUser] = Eq.by(_.id)
}

/** Service users have the strongest `Role` and represent services themselves. */
final case class ServiceUser(id: User.Id, name: String) extends User {
  val role        = ServiceRole(name)
  val displayName = s"Service User ($name)"
}

object ServiceUser {
  implicit val eqServiceUser: Eq[ServiceUser] = Eq.by(x => (x.id, x.name))
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

object StandardUser {
  implicit val eqStandardUser: Eq[StandardUser] =
    Eq.by(x => (x.id, x.role, x.otherRoles, x.profile))
}
