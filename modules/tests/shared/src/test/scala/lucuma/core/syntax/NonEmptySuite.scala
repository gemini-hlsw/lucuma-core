// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package syntax

import cats.data.NonEmptyList
import org.scalacheck.Prop._

import nonempty.*

class NonEmptySuite extends munit.ScalaCheckSuite {

  test("toZipperMax") {
    forAll { (h: Int, t: List[Int]) =>
      val nel = NonEmptyList(h, t)
      val z   = nel.toZipperMax
      assertEquals(z.focus, nel.toList.max)
      assertEquals(z.toNel, nel)
    }
  }

  test("toZipperMin") {
    forAll { (h: Int, t: List[Int]) =>
      val nel = NonEmptyList(h, t)
      val z   = nel.toZipperMin
      assertEquals(z.focus, nel.toList.min)
      assertEquals(z.toNel, nel)
    }
  }
}
