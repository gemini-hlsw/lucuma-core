// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import io.circe._
import java.net.URI

/**
 * The ORCID iD is an https URI with a 16-digit number that is compatible with the ISO Standard
 * (ISO 27729), also known as the International Standard Name Identifier (ISNI). The last digit
 * is a checksum that is either 0-9, or X.
 * @see https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier
 */
sealed abstract case class OrcidId(value: URI)
object OrcidId {

  private val Pat = raw"https://orcid.org/(\d{4})-(\d{4})-(\d{4})-(\d{3})([\dX])".r

  def fromUri(uri: URI): Either[String, OrcidId] =
    fromString(uri.toString)

  def fromString(s: String): Either[String, OrcidId] =
    s match {
      case Pat(a, b, c, d, x) =>
        if (checkDigit(a + b + c + d) == x)
          Right(new OrcidId(new URI(s)) {})
        else
          Left(s"Invalid ORCID iD (bad checksum): $s")
      case _ => Left(s"Invalid ORCID iD: $s")
    }

  implicit val EqOrcid: Eq[OrcidId] =
    Eq.by(_.value.toString)

  implicit val encoder: Encoder[OrcidId] =
    Encoder[String].contramap(_.value.toString)

  implicit val decoder: Decoder[OrcidId] =
    Decoder[String].emap(s => fromString(s))

  // adapted from the code at the link above
  def checkDigit(baseDigits: String): String = {
    require(baseDigits.forall(c => c >= '0' && c <= '9'))
    val total = baseDigits.foldLeft(0) { (acc, c) =>
      val digit = c - '0'
      (acc + digit) * 2
    }
    val remainder = total % 11
    val result    = (12 - remainder) % 11
    if (result == 10) "X" else result.toString
  }

}
