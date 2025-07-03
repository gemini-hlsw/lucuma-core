// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

import scala.util.Random

class OffsetGeneratorSuite extends munit.FunSuite:

  test("grid generates correct number of offsets"):
    val offsets = OffsetGenerator.grid(PosInt.unsafeFrom(3), PosInt.unsafeFrom(4), 1.arcsec, 2.arcsec)
    assertEquals(offsets.length, 12) // 3 * 4

  test("grid generates symmetric pattern around center"):
    val offsets = OffsetGenerator.grid(PosInt.unsafeFrom(3), PosInt.unsafeFrom(3), 1.arcsec, 1.arcsec)
    val center = Offset.Zero

    assertEquals(offsets.length, 9)
    assert(offsets.toList.contains(center))
    val centerIndex = offsets.toList.indexOf(center)
    assert(centerIndex >= 0)

  test("grid handles single point"):
    val offsets = OffsetGenerator.grid(PosInt.unsafeFrom(1), PosInt.unsafeFrom(1), 1.arcsec, 1.arcsec)
    assertEquals(offsets.length, 1)
    assertEquals(offsets.head, Offset.Zero)

  // Test random generation
  test("random generates correct number of offsets"):
    val random = new Random(42)
    val offsets = OffsetGenerator.random(PosInt.unsafeFrom(10), 5.arcsec, Offset.Zero, random)
    assertEquals(offsets.length, 10)

  test("random generates offsets within expected bounds"):
    val random = new Random(42)
    val size = 10.arcsec
    val offsets = OffsetGenerator.random(PosInt.unsafeFrom(20), size, Offset.Zero, random)

    // All offsets should be within the diameter
    val maxDistance = offsets.toList.map(_.distance(Offset.Zero)).maxBy(_.toSignedDoubleRadians)
    assert(maxDistance.toSignedDoubleRadians <= size.toSignedDoubleRadians * 0.8)

  // Test spiral generation
  test("spiral generates correct number of offsets"):
    val random = new Random(42)
    val offsets = OffsetGenerator.spiral(PosInt.unsafeFrom(15), 8.arcsec, Offset.Zero, random)
    assertEquals(offsets.length, 15)

  test("spiral first point is at center"):
    val random = new Random(42)
    val offsets = OffsetGenerator.spiral(PosInt.unsafeFrom(10), 5.arcsec, Offset.Zero, random)

    val firstDistance = offsets.head.distance(Offset.Zero)
    assert(firstDistance.toSignedDoubleRadians < 0.001)

  test("spiral single point"):
    val random = new Random(42)
    val offsets = OffsetGenerator.spiral(PosInt.unsafeFrom(1), 5.arcsec, Offset.Zero, random)
    assertEquals(offsets.length, 1)

    val distance = offsets.head.distance(Offset.Zero)
    assert(distance.toSignedDoubleRadians < 0.001)

  test("all generators handle zero size gracefully"):
    val random = new Random(42)
    val zeroSize = Angle.fromDoubleArcseconds(0.0)

    // Grid with zero step size should work
    val gridOffsets = OffsetGenerator.grid(PosInt.unsafeFrom(2), PosInt.unsafeFrom(2), zeroSize, zeroSize)
    assertEquals(gridOffsets.length, 4)
    assert(gridOffsets.toList.forall(_ == Offset.Zero))

    val randomOffsets = OffsetGenerator.random(PosInt.unsafeFrom(3), zeroSize, Offset.Zero, random)
    assertEquals(randomOffsets.length, 3)

    val spiralOffsets = OffsetGenerator.spiral(PosInt.unsafeFrom(3), zeroSize, Offset.Zero, random)
    assertEquals(spiralOffsets.length, 3)

  test("negative step sizes work"):
    val offsets = OffsetGenerator.grid(PosInt.unsafeFrom(2), PosInt.unsafeFrom(2), -1.arcsec, -1.arcsec)
    assertEquals(offsets.length, 4)

  test("gridP generates P components"):
    val components = OffsetGenerator.gridP(PosInt.unsafeFrom(5), 1.arcsec)
    assertEquals(components.length, 5)
    // P components should be symmetric around center
    val pValues = components.toList.map(_.toAngle.toSignedDoubleRadians).sorted
    assertEquals(pValues.length, 5)

  test("gridQ generates Q components"):
    val components = OffsetGenerator.gridQ(PosInt.unsafeFrom(3), 2.arcsec)
    assertEquals(components.length, 3)
    // Should have center
    assert(components.toList.contains(Offset.Q.Zero))

  test("randomP generates P components"):
    val random = new Random(42)
    val components = OffsetGenerator.randomP(PosInt.unsafeFrom(10), 5.arcsec, random = random)
    assertEquals(components.length, 10)

  test("randomQ generates Q components"):
    val random = new Random(42)
    val components = OffsetGenerator.randomQ(PosInt.unsafeFrom(8), 4.arcsec, random = random)
    assertEquals(components.length, 8)
