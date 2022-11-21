// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.arb._
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbAngle {

  implicit def arbAngle: Arbitrary[Angle] =
    Arbitrary(arbitrary[Double].map(Angle.fromDoubleDegrees))

  implicit def arbHourAngle: Arbitrary[HourAngle] =
    Arbitrary(arbitrary[Double].map(HourAngle.fromDoubleHours))

  implicit def arbDMS: Arbitrary[Angle.DMS] =
    Arbitrary(arbitrary[Angle].map(Angle.dms.get))

  implicit def arbHMS: Arbitrary[HourAngle.HMS] =
    Arbitrary(arbitrary[HourAngle].map(HourAngle.hms.get))

  implicit def cogAngle: Cogen[Angle] =
    Cogen[Double].contramap(_.toDoubleDegrees)

  implicit def cogHourAngle: Cogen[HourAngle] =
    Cogen[Double].contramap(_.toDoubleDegrees)

  implicit def cogDMS: Cogen[Angle.DMS] =
    Cogen[Angle].contramap(_.toAngle)

  implicit def cogHMS: Cogen[HourAngle.HMS] =
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
      .flatMapOneOf(Gen.const, perturbations: _*)

  // Strings that are often parsable as DMS.
  val stringsDMS: Gen[String] =
    arbitrary[Angle].map(Angle.fromStringDMS.reverseGet).flatMapOneOf(Gen.const, perturbations: _*)

  // Strings that are often parsable as signed DMS.
  val stringsSignedDMS: Gen[String] =
    arbitrary[Angle]
      .map(Angle.fromStringSignedDMS.reverseGet)
      .flatMapOneOf(Gen.const, perturbations: _*)

}

object ArbAngle extends ArbAngle
