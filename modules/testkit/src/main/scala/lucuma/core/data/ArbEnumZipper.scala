// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data
package arb

import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen.*

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
