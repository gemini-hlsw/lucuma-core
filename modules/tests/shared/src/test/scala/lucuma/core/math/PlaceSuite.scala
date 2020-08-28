// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.implicits._
import cats.Show
import coulomb.scalacheck.ArbQuantity._
import coulomb.cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.cats._
import lucuma.core.math.arb._
import monocle.law.discipline._
import io.chrisdavenport.cats.time._
import org.scalacheck.Prop._
import munit.DisciplineSuite
import cats.kernel.laws.discipline.EqTests

final class PlaceSuite extends DisciplineSuite {
  import ArbPlace._
  import ArbDeclination._
  import ArbAngle._
  import ArbTime._

  checkAll("Place", EqTests[Place].eqv)

  test("Show must be natural") {
    forAll { a: Place =>
      assertEquals(a.toString, Show[Place].show(a))
    }
  }

  checkAll("Place.latitude", LensTests(Place.latitude))
  checkAll("Place.longitude", LensTests(Place.longitude))
  checkAll("Place.altitude", LensTests(Place.altitude))
  checkAll("Place.timezone", LensTests(Place.timezone))
}
