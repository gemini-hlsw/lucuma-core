// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import cats.implicits.*
import monocle.Focus
import monocle.Lens

/** A user's name and email. */
final case class UserProfile(
  givenName:  Option[String],
  familyName: Option[String],
  creditName: Option[String],
  email:      Option[String]
):

  /** The best display name we can provide, based on available information. */
  def displayName: Option[String] =
    creditName                                       <+>
    (givenName, familyName).mapN((g, f) => s"$g $f") <+>
    familyName                                       <+>
    givenName

object UserProfile:

  val Empty: UserProfile =
    UserProfile(None, None, None, None)

  val givenName: Lens[UserProfile, Option[String]]  = Focus[UserProfile](_.givenName)
  val familyName: Lens[UserProfile, Option[String]] = Focus[UserProfile](_.familyName)
  val creditName: Lens[UserProfile, Option[String]] = Focus[UserProfile](_.creditName)
  val email: Lens[UserProfile, Option[String]]      = Focus[UserProfile](_.email)

  given Eq[UserProfile] =
    Eq.by(x => (x.givenName, x.familyName, x.creditName, x.email))
