// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data
package arb

import lucuma.core.data.EnumZipper
import lucuma.core.util.Enumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{ Arbitrary, Cogen }
import org.scalacheck.Gen._
import lucuma.core.util.arb.ArbEnumerated._

trait ArbEnumZipper {
  implicit def arbEnumZipper[A: Enumerated]: Arbitrary[EnumZipper[A]] =
    Arbitrary(
      for {
        z <- const(EnumZipper.of[A])
        a <- arbitrary[A]
      } yield z.withFocus(a)
    )

  implicit def enumZipperCogen[A: Enumerated]: Cogen[EnumZipper[A]] =
    Cogen[A].contramap(_.focus)
}

object ArbEnumZipper extends ArbEnumZipper
