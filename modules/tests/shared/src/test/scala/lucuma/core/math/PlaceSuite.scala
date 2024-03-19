// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Show
import cats.kernel.laws.discipline.EqTests
import coulomb.ops.algebra.cats.all.given
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.arb.*
import lucuma.core.arb.ArbTime
import lucuma.core.math.arb.*
import monocle.law.discipline.*
import munit.DisciplineSuite
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

final class PlaceSuite extends DisciplineSuite {
  import ArbPlace.given
  import ArbDeclination.given
  import ArbAngle.given
  import ArbTime.given
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
