// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package syntax

import cats.data.NonEmptyList
import org.scalacheck.Prop._

import nonempty.*

class NonEmptySuite extends munit.ScalaCheckSuite {

  test("focusMax") {
    forAll { (h: Int, t: List[Int]) =>
      val nel = NonEmptyList(h, t)
      val z   = nel.focusMax
      assertEquals(z.focus, nel.toList.max)
      assertEquals(z.toNel, nel)
    }
  }

  test("focusMin") {
    forAll { (h: Int, t: List[Int]) =>
      val nel = NonEmptyList(h, t)
      val z   = nel.focusMin
      assertEquals(z.focus, nel.toList.min)
      assertEquals(z.toNel, nel)
    }
  }
}
