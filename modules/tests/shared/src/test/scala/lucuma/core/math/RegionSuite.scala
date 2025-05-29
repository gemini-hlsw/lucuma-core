// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.arb.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*
import monocle.law.discipline.OptionalTests

final class RegionSuite extends munit.DisciplineSuite:
  import ArbArc.given
  import ArbDeclination.given
  import ArbRegion.given
  import ArbRightAscension.given
  import ArbCoordinates.given

  val label = s"Region"
  
  checkAll(label, EqTests[Region].eqv)
  checkAll(label, OptionalTests(Region.raArc))
  checkAll(label, OptionalTests(Region.raArcStart))
  checkAll(label, OptionalTests(Region.raArcEnd))
  checkAll(label, OptionalTests(Region.decArc))
  checkAll(label, OptionalTests(Region.decArcStart))
  checkAll(label, OptionalTests(Region.decArcEnd))

  test(s"$label: Equality must be natural"):
    forAll: (a: Region, b: Region) =>
      assertEquals(a.equals(b), Eq[Region].eqv(a, b))

  test(s"$label: The empty region contains no coordinates."):
    forAll: (a: Coordinates) =>
      assert(!Region.Empty.contains(a))

  test(s"$label: The full region contains every coordinate."):
    forAll: (a: Coordinates) =>
      assert(Region.Full.contains(a))

  test(s"$label: A region contains coordinates if its arcs do."):
    forAll: (r: Region, a: Coordinates) =>
      assertEquals(r.contains(a), r.raArc.contains(a.ra) && r.decArc.contains(a.dec))

  test(s"$label: A region containsAll of another region if each of its arcs containsAll of the others' arcs."):
    forAll: (a: Region, b: Region) =>
      assertEquals(a.containsAll(b), a.raArc.containsAll(b.raArc) && a.decArc.containsAll(b.decArc))

  test(s"$label: existsOverlap commutes"):
    forAll: (a: Region, b: Region) =>
      assertEquals(a.existsOverlap(b), b.existsOverlap(a))
    
  test(s"$label: There existsOverlap between regions if there existsOverlap between their arcs"):
    forAll: (a: Region, b: Region) =>
      assertEquals(a.existsOverlap(b), a.raArc.existsOverlap(b.raArc) && a.decArc.existsOverlap(b.decArc))
    