// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.data.NonEmptyChain
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbObservationValidation {
  import ArbEnumerated.given

  given Arbitrary[NonEmptyChain[String]] =
    Arbitrary(
      for {
        s  <- arbitrary[String]
        ss <- arbitrary[List[String]]
      } yield NonEmptyChain.of(s, ss*)
    )

  given Cogen[NonEmptyChain[String]] =
    Cogen[(String, List[String])].contramap { a =>
      (a.head, a.tail.toList)
    }

  given Arbitrary[ObservationValidation] =
    Arbitrary {
      for {
        c <- arbitrary[ObservationValidationCode]
        m <- arbitrary[NonEmptyChain[String]]
      } yield ObservationValidation(c, m)
    }

  given Cogen[ObservationValidation] =
    Cogen[(ObservationValidationCode, NonEmptyChain[String])].contramap { ov =>
      (ov.code, ov.messages)
    }
}

object ArbObservationValidation extends ArbObservationValidation
