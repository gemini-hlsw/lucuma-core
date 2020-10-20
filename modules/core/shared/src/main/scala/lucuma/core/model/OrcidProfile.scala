// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import cats.implicits._

/** An ORCID profile is an OrcidId and a set of optional fields. */
final case class OrcidProfile(
  orcidId:      OrcidId,
  givenName:    Option[String],
  familyName:   Option[String],
  creditName:   Option[String],
  primaryEmail: Option[String],
) {

  /** The best display name we can provide, based on available information. */
  def displayName: String = (
    creditName                                       <+>
    (givenName, familyName).mapN((g, f) => s"$g $f") <+>
    familyName                                       <+>
    givenName
  ).getOrElse(orcidId.value.toString())

}

object OrcidProfile {
  implicit val eqOrcidProfile: Eq[OrcidProfile] =
    Eq.by(x => (x.orcidId, x.givenName, x.familyName, x.creditName, x.primaryEmail))
}
