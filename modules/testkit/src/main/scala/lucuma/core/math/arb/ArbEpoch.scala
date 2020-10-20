// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import java.time.LocalDateTime

import lucuma.core.arb.ArbTime
import lucuma.core.arb._
import lucuma.core.math.Epoch
import lucuma.core.math.Epoch.Julian
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck._

trait ArbEpoch {
  import ArbTime._

  implicit val arbScheme: Arbitrary[Epoch.Scheme] =
    Arbitrary(oneOf(Epoch.Julian, Epoch.Besselian))

  implicit val arbEpoch: Arbitrary[Epoch] =
    Arbitrary {
      for {
        sch <- arbitrary[Epoch.Scheme]
        ldt <- arbitrary[LocalDateTime]
      } yield sch.fromLocalDateTime(ldt)
    }

  implicit val cogEpoch: Cogen[Epoch] =
    Cogen[String].contramap(Epoch.fromString.reverseGet)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace("2", "0")) // create a leading zero, maybe (ok)
    )

  // Strings that are often parsable as epoch
  val strings: Gen[String] =
    arbitrary[Epoch].map(Epoch.fromString.reverseGet).flatMapOneOf(Gen.const, perturbations: _*)

  val stringsNoScheme: Gen[String] =
    arbitrary[Epoch]
      .map(Epoch.fromStringNoScheme.reverseGet)
      .flatMapOneOf(Gen.const, perturbations: _*)

  val arbJulianEpoch: Arbitrary[Epoch] =
    Arbitrary {
      arbitrary[LocalDateTime].map(Julian.fromLocalDateTime)
    }
}

object ArbEpoch extends ArbEpoch
