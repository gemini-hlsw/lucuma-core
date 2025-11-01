// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.refineV
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.arb.*
import lucuma.core.math.Epoch
import lucuma.core.math.Epoch.Besselian
import lucuma.core.math.Epoch.Julian
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.*

trait ArbEpoch {
  given Arbitrary[Epoch.Scheme] =
    Arbitrary(oneOf(Epoch.Julian, Epoch.Besselian))

  given Arbitrary[Epoch] =
    Arbitrary {
      for {
        sch <- arbitrary[Epoch.Scheme]
        mys <- arbitrary[Epoch.IntMilliYear]
      } yield sch.fromMilliyears(mys)
    }

  given Cogen[Epoch] =
    Cogen[String].contramap(Epoch.fromString.reverseGet)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],   // swap for a random string
      s => Gen.const(s.drop(1)) // strip the scheme - won't parse
    )

  private val noSchemePerturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],                // swap for a random string
      s => Gen.const(s"${s}0"),              // test normalization with trailing zeros
      s => Gen.const(s"${Julian.prefix}$s"), // add the schemes, should not parse
      s => Gen.const(s"${Besselian.prefix}$s")
    )

  // Strings that are often parsable as epoch
  val strings: Gen[String] =
    arbitrary[Epoch]
      .map(Epoch.fromString.reverseGet)
      .flatMapOneOf(Gen.const, perturbations*) // include random strings

  val stringsNoScheme: Gen[String] =
    arbitrary[Epoch]
      .map(Epoch.fromStringNoScheme.reverseGet)
      .flatMapOneOf(Gen.const, noSchemePerturbations*)

  val arbJulianEpoch: Arbitrary[Epoch] =
    Arbitrary {
      arbitrary[Epoch.IntMilliYear].map(Julian.fromMilliyears)
    }

  // Generator for epochs with 2 decimal places
  val epochs2Decimal: Gen[Epoch] =
    for {
      scheme     <- arbitrary[Epoch.Scheme]
      year       <- Gen.choose(1900, 3000)
      hundredths <- Gen.choose(0, 99)
      milliyears = year * 1000 + hundredths * 10
      intMilliYear = refineV[Epoch.MilliYear](milliyears).toOption.get
    } yield scheme.fromMilliyears(intMilliYear)

  val arbEpoch2Decimal: Arbitrary[Epoch] = Arbitrary(epochs2Decimal)

}

object ArbEpoch extends ArbEpoch
