// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data

import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.FunctorTests
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.data.arb.ArbPerSite.given

final class PerSiteSuite extends munit.DisciplineSuite {
  checkAll("Functor[PerSite]", FunctorTests[PerSite].functor[Int, Int, Int])
  checkAll("Eq[PerSite]", EqTests[PerSite[Int]].eqv)
  checkAll("Codec[PerSite]", CodecTests[PerSite[Int]].codec)
}
