// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.implicits._
import io.circe._

/** ORCID iD */
sealed abstract case class OrcidId(value: String)
object OrcidId {

  private val Pat = """^(\d{4}-){3,}\d{3}[\dX]$""".r

  def fromString(s: String): Option[OrcidId] =
    Pat.findFirstIn(s).map(new OrcidId(_) {})

  implicit val EqOrcid: Eq[OrcidId] =
    Eq.by(_.value)

  implicit val encoder: Encoder[OrcidId] =
    Encoder[String].contramap(_.value)

  implicit val decoder: Decoder[OrcidId] =
    Decoder[String].emap(s => fromString(s).toRight(s"Invalid ORCID iD: $s"))

}
