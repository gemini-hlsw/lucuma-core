// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.config

import io.circe.*

import scala.util.matching.Regex

sealed abstract case class Email(value: String)

object Email {

  val regex: Regex =
    // https://stackoverflow.com/questions/201323/how-to-validate-an-email-address-using-a-regular-expression
    """(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])""".r

  def dummy: Email =
    apply("dummy@example.org").toOption.get

  def apply(value: String): Either[String, Email] =
    if (regex.pattern.matcher(value).matches) Right(new Email(value) {})
    else Left(s"Invalid email address: $value")

  implicit val decoder: Decoder[Email] = Decoder[String].emap(apply)
  implicit val encoder: Encoder[Email] = Encoder[String].contramap(_.value)

}