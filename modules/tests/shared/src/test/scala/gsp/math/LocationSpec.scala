// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import gsp.math.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
object LocationSpec extends CatsSuite {
  import ArbLocation._
  import ArbDeclination._
  import ArbAngle._

  checkAll("Location.latitude", LensTests(Location.latitude))
  checkAll("Location.longitude", LensTests(Location.longitude))
  checkAll("Location.altitude", LensTests(Location.altitude))

  test("Equality must be natural") {
    forAll { (a: Location, b: Location) =>
      a.equals(b) shouldEqual Eq[Location].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Location) =>
      a.toString shouldEqual Show[Location].show(a)
    }
  }
}
