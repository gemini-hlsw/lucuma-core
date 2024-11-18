// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import cats.implicits.*

/**
 * An ORCID profile is an OrcidId and an ORCID user profile or else a fallback
 * profile.
 */
final case class OrcidProfile(
  orcidId:  OrcidId,
  primary:  UserProfile,
  fallback: UserProfile
):

  /** The best display name we can provide, based on available information. */
  def displayName: String = (
    primary.displayName <+> fallback.displayName
  ).getOrElse(orcidId.value.toString)

  /** The orcid primary email, or else the fallback email (if any). */
  def email: Option[String] =
    primary.email <+> fallback.email

object OrcidProfile:
  given Eq[OrcidProfile] =
    Eq.by(x => (x.orcidId, x.primary, x.fallback))