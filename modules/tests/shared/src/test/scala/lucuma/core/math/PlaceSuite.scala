// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Show
import cats.kernel.laws.discipline.EqTests
import coulomb.cats.quantity.given
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.arb.ArbTime
import lucuma.core.arb._
import lucuma.core.math.arb._
import monocle.law.discipline._
import munit.DisciplineSuite
import org.scalacheck.Prop._
import org.typelevel.cats.time._

final class PlaceSuite extends DisciplineSuite {
  import ArbPlace._
  import ArbDeclination._
  import ArbAngle._
  import ArbTime._
  import ArbRefined.given
  import ArbQuantity.given

  checkAll("Place", EqTests[Place].eqv)

  test("Show must be natural") {
    forAll { (a: Place) =>
      assertEquals(a.toString, Show[Place].show(a))
    }
  }

  checkAll("Place.latitude", LensTests(Place.latitude))
  checkAll("Place.longitude", LensTests(Place.longitude))
  checkAll("Place.altitude", LensTests(Place.altitude))
  checkAll("Place.timezone", LensTests(Place.timezone))
}
