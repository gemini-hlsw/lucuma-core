// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq

/**
 * An ORCID profile is an OrcidId and a user profile.
 */
final case class OrcidProfile(
  orcidId: OrcidId,
  profile: UserProfile
):

  /** The best display name we can provide, based on available information. */
  def displayName: String =
    profile.displayName.getOrElse(orcidId.value.toString)

  /** The orcid primary email, or else the fallback email (if any). */
  def email: Option[String] =
    profile.email

object OrcidProfile:
  given Eq[OrcidProfile] =
    Eq.by(x => (x.orcidId, x.profile))