// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.arb.*
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*
import org.scalacheck.*

trait ArbAngle {

  given Arbitrary[Angle] =
    Arbitrary(arbitrary[Double].map(Angle.fromDoubleDegrees))

  given Arbitrary[HourAngle] =
    Arbitrary(arbitrary[Double].map(HourAngle.fromDoubleHours))

  given Arbitrary[Angle.DMS] =
    Arbitrary(arbitrary[Angle].map(Angle.dms.get))

  given Arbitrary[HourAngle.HMS] =
    Arbitrary(arbitrary[HourAngle].map(HourAngle.hms.get))

  given Cogen[Angle] =
    Cogen[Double].contramap(_.toDoubleDegrees)

  given Cogen[HourAngle] =
    Cogen[Double].contramap(_.toDoubleDegrees)

  given Cogen[Angle.DMS] =
    Cogen[Angle].contramap(_.toAngle)

  given Cogen[HourAngle.HMS] =
    Cogen[HourAngle].contramap(_.toHourAngle)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],             // swap for a random string
      s => Gen.const(s.replace(":", " ")) // replace colons with spaces (ok)
    )

  // Strings that are often parsable as HMS.
  val stringsHMS: Gen[String] =
    arbitrary[HourAngle]
      .map(HourAngle.fromStringHMS.reverseGet)
      .flatMapOneOf(Gen.const, perturbations*)

  // Strings that are often parsable as DMS.
  val stringsDMS: Gen[String] =
    arbitrary[Angle].map(Angle.fromStringDMS.reverseGet).flatMapOneOf(Gen.const, perturbations*)

  // Strings that are often parsable as signed DMS.
  val stringsSignedDMS: Gen[String] =
    arbitrary[Angle]
      .map(Angle.fromStringSignedDMS.reverseGet)
      .flatMapOneOf(Gen.const, perturbations*)

}

object ArbAngle extends ArbAngle
