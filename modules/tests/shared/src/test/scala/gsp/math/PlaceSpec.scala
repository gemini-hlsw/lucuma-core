// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import gsp.math.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
object PlaceSpec extends CatsSuite {
  import ArbPlace._
  import ArbDeclination._
  import ArbAngle._

  checkAll("Place.latitude", LensTests(Place.latitude))
  checkAll("Place.longitude", LensTests(Place.longitude))
  checkAll("Place.altitude", LensTests(Place.altitude))

  test("Equality must be natural") {
    forAll { (a: Place, b: Place) =>
      a.equals(b) shouldEqual Eq[Place].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Place) =>
      a.toString shouldEqual Show[Place].show(a)
    }
  }
}
