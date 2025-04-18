// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data.arb

import lucuma.core.data.EmailAddress
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbEmailAddress {

  val samples = List(
    "email@example.com",
    "firstname.lastname@example.com",
    "email@subdomain.example.com",
    "firstname+lastname@example.com",
    "email@123.123.123.123",
    "email@[123.123.123.123]",
    """"email"@example.com""",
    "1234567890@example.com",
    "email@example-one.com",
    "_______@example.com",
    "email@example.name",
    "email@example.museum",
    "email@example.co.jp",
    "firstname-lastname@example.com",
    """much.”more\ unusual”@example.com""",
    """very.unusual.”@”.unusual.com@example.com""",
    """very.”(),:;<>[]”.VERY.”very@\\ "very”.unusual@strange.example.com"""
  )

  given Arbitrary[EmailAddress] =
    Arbitrary {
        Gen.oneOf(samples).map(EmailAddress.unsafeFrom)
    }

  given Cogen[EmailAddress] =
    Cogen[String].contramap(_.value.toString)

}

object ArbEmailAddress extends ArbEmailAddress
