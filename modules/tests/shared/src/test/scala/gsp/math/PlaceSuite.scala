// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.{ Eq, Show }
import cats.implicits._
import coulomb.scalacheck.ArbQuantity._
import coulomb.cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.cats._
import gsp.math.arb._
import monocle.law.discipline._
import munit._
import org.scalacheck.Prop._

class PlaceSuite extends FunSuite with DisciplineSuite with ScalaCheckSuite {
  import ArbPlace._
  import ArbDeclination._
  import ArbAngle._

  checkAll("Place.latitude", LensTests(Place.latitude))
  checkAll("Place.longitude", LensTests(Place.longitude))
  checkAll("Place.altitude", LensTests(Place.altitude))

  test("Equality must be natural") {
    forAll { (a: Place, b: Place) =>
      assertEquals(a.equals(b), Eq[Place].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Place) =>
      assertEquals(a.toString, Show[Place].show(a))
    }
  }
}
