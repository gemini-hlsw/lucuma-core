// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.arb.*
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.model.Ephemeris
import lucuma.core.model.Ephemeris.Key.*
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbEphemerisKey {

  private def genStringDes[A](f: String => A): Gen[A] =
    Gen.alphaNumStr.map(s => f(s.take(10)))

  private def genIntDes[A](f: Int => A): Gen[A] =
    arbitrary[Int].map(f)

  given Arbitrary[Ephemeris.Key] =
    Arbitrary {
      Gen.oneOf[Ephemeris.Key](
        genStringDes(Comet.apply),
        genStringDes(AsteroidNew.apply),
        genIntDes(AsteroidOld.apply),
        genIntDes(MajorBody.apply),
        genIntDes(UserSupplied.apply)
      )
    }

  given Cogen[Ephemeris.Key] =
    Cogen[String].contramap(Ephemeris.Key.fromString.reverseGet)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace("2", "0")) // create a leading zero, perhaps
    )

  // Key and des pairs that are often parsable
  val keyAndDes: Gen[(EphemerisKeyType, String)] =
    for {
      k <- arbitrary[EphemerisKeyType]
      d <- arbitrary[Int].map(_.abs.toString).flatMapOneOf(Gen.const, perturbations*)
    } yield (k, d)

  // Strings that are often parsable
  val strings: Gen[String] =
    arbitrary[Ephemeris.Key]
      .map(Ephemeris.Key.fromString.reverseGet)
      .flatMapOneOf(Gen.const, perturbations*)

}

object ArbEphemerisKey extends ArbEphemerisKey
