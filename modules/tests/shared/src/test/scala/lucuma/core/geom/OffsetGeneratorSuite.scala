// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.effect.IO
import cats.effect.std.Random
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.refined.*
import munit.CatsEffectSuite

class OffsetGeneratorSuite extends CatsEffectSuite:

  test("grid generates correct number of offsets"):
    assertIO(
      OffsetGenerator.grid[IO](3.refined, 4.refined, 1.arcsec, 2.arcsec).map(_.length),
      12 // 3 * 4
    )

  test("grid generates symmetric pattern around center"):
    OffsetGenerator.grid[IO](3.refined, 3.refined, 1.arcsec, 1.arcsec).map: offsets =>
      assertEquals(offsets.length, 9)
      assert(offsets.toList.contains(Offset.Zero))
      val centerIndex = offsets.toList.indexOf(Offset.Zero)
      assert(centerIndex >= 0)

  test("grid handles single point"):
    OffsetGenerator.grid[IO](1.refined, 1.refined, 1.arcsec, 1.arcsec).map: offsets =>
      assertEquals(offsets.length, 1)
      assertEquals(offsets.head, Offset.Zero)

  // Test random generation
  test("random generates correct number of offsets"):
    for
      given Random[IO] <- Random.scalaUtilRandomSeedInt[IO](42)
      offsets          <- OffsetGenerator.random[IO](10.refined, 5.arcsec)
    yield assertEquals(offsets.length, 10)

  test("random generates offsets within expected bounds"):
    val size = 10.arcsec
    for
      given Random[IO] <- Random.scalaUtilRandomSeedInt[IO](42)
      offsets          <- OffsetGenerator.random[IO](20.refined, size)
    yield
      // All offsets should be within the diameter
      val maxDistance = offsets.toList.map(_.distance(Offset.Zero)).maxBy(_.toSignedDoubleRadians)
      assert(maxDistance.toSignedDoubleRadians <= size.toSignedDoubleRadians * 0.8)

  // Test spiral generation
  test("spiral generates correct number of offsets"):
    assertIO(
      for
        given Random[IO] <- Random.scalaUtilRandomSeedInt[IO](42)
        offsets          <- OffsetGenerator.spiral[IO](15.refined, 8.arcsec)
      yield offsets.length,
      15
    )

  test("spiral first point is at center"):
    for
      given Random[IO] <- Random.scalaUtilRandomSeedInt[IO](42)
      offsets          <- OffsetGenerator.spiral[IO](10.refined, 5.arcsec)
    yield
      val firstDistance = offsets.head.distance(Offset.Zero)
      assert(firstDistance.toSignedDoubleRadians < 0.001)

  test("spiral single point"):
    for
      given Random[IO] <- Random.scalaUtilRandomSeedInt[IO](42)
      offsets          <- OffsetGenerator.spiral[IO](1.refined, 5.arcsec)
    yield
      assertEquals(offsets.length, 1)
      val distance = offsets.head.distance(Offset.Zero)
      assert(distance.toSignedDoubleRadians < 0.001)

  test("negative step sizes work"):
    assertIO(
      OffsetGenerator.grid[IO](2.refined, 2.refined, -1.arcsec, -1.arcsec).map(_.length),
      4
    )

  test("gridP generates P components"):
    OffsetGenerator.gridP[IO](5.refined, 1.arcsec).map: c =>
      assertEquals(c.length, 5)
      // P components should be symmetric around center
      val pValues = c.toList.map(_.toAngle.toSignedDoubleRadians).sorted
      assertEquals(pValues.length, 5)

  test("gridQ generates Q components"):
    OffsetGenerator.gridQ[IO](3.refined, 2.arcsec).map: c =>
      assertEquals(c.length, 3)
      // Should have center
      assert(c.toList.contains(Offset.Q.Zero))

  test("randomP generates P components"):
    for
      given Random[IO] <- Random.scalaUtilRandomSeedInt[IO](42)
      components       <- OffsetGenerator.randomP[IO](10.refined, 5.arcsec)
    yield assertEquals(components.length, 10)

  test("randomQ generates Q components"):
    for
      given Random[IO] <- Random.scalaUtilRandomSeedInt[IO](42)
      components       <- OffsetGenerator.randomQ[IO](8.refined, 4.arcsec)
    yield assertEquals(components.length, 8)
