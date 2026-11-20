// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.syntax.refined.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class RefinedSuite extends ScalaCheckSuite:

  property("NonNegInt saturatingAdd is addition capped at Int.MaxValue"):
    forAll: (a: NonNegInt, b: NonNegInt) =>
      val expected = (a.value.toLong + b.value).min(Int.MaxValue)
      assertEquals(a.saturatingAdd(b).value.toLong, expected)

  property("NonNegInt +| is saturatingAdd"):
    forAll: (a: NonNegInt, b: NonNegInt) =>
      assertEquals(a +| b, a.saturatingAdd(b))

  test("NonNegInt +| saturates"):
    assertEquals(NonNegInt.MaxValue +| NonNegInt.MaxValue, NonNegInt.MaxValue)
