// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbZipper {

  def arbZipper[A: Arbitrary](limit: Int): Arbitrary[Zipper[A]] =
    Arbitrary {
      for {
        f  <- arbitrary[A]
        ll <- Gen.choose(0, limit)
        l  <- Gen.listOfN(ll, arbitrary[A])
        rl <- Gen.choose(0, limit)
        r  <- Gen.listOfN(rl, arbitrary[A])
      } yield Zipper(l, f, r)
    }

  given[A: Arbitrary]: Arbitrary[Zipper[A]] =
    arbZipper(100)

  given[A: Cogen]: Cogen[Zipper[A]] =
    Cogen[(List[A], A, List[A])].contramap(z => (z.lefts, z.focus, z.rights))

}

object ArbZipper extends ArbZipper
