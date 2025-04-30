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

trait ArbPerSite:

  def genPerSite[A: Arbitrary]: Gen[PerSite[A]] =
    arbitrary[(A,A)].map: (a1, a2) =>
      PerSite(a1, a2)

  given[A: Arbitrary]: Arbitrary[PerSite[A]] =
    Arbitrary(genPerSite)

  given[A: Cogen]: Cogen[PerSite[A]] =
    Cogen[(A, A)].contramap(_.toPair)

object ArbPerSite extends ArbPerSite
