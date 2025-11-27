// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.effect.IO
import cats.effect.std.Random
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.refined.auto.*
import munit.CatsEffectSuite

class OffsetGeneratorSuite extends CatsEffectSuite:

  def offset(pµas: Long, qµas: Long): Offset =
    Offset.signedMicroarcseconds.reverseGet((pµas, qµas))

  def row(qµas: Long)(pµas: Long*): List[Offset] =
    pµas.toList.map(p => offset(p, qµas))

  test("uniform is correct when the region is empty"):
    val offsets = OffsetGenerator.uniform(5.refined, Offset.Zero, Offset.Zero)
    assertEquals(offsets.toList, List(Offset.Zero, Offset.Zero, Offset.Zero, Offset.Zero, Offset.Zero))

  test("uniform is correct when the region has no width"):
    val offsets = OffsetGenerator.uniform(5.refined, Offset.Zero, offset(0L, 10_000_000L))
    assertEquals(
      offsets.toList,
      List(
        offset(0L, 10_000_000L),
        offset(0L,  7_500_000L),
        offset(0L,  5_000_000L),
        offset(0L,  2_500_000L),
        offset(0L,          0L)
      )
    )

  test("uniform is correct when the region has no height"):
    val offsets = OffsetGenerator.uniform(5.refined, Offset.Zero, offset(10_000_000L, 0L))
    assertEquals(
      offsets.toList,
      List(
        offset(10_000_000L, 0L),
        offset( 7_500_000L, 0L),
        offset( 5_000_000L, 0L),
        offset( 2_500_000L, 0L),
        offset(         0L, 0L)
      )
    )

  test("uniform selects the upper left corner"): // recall p increases to the left
    val offsets = OffsetGenerator.uniform(1.refined, offset(10_000_000L, 5_000_000L), offset(20_000_000L, 0L))
    assertEquals(offsets.toList, List(offset(20_000_000L, 5_000_000L)))

  test("uniform respects the aspect ratio (1:1)"):
    val offsets = OffsetGenerator.uniform(4.refined, offset(10_000_000L, 10_000_000L), offset(20_000_000L, 20_000_000L))
    assertEquals(
      offsets.toList,
      row(20_000_000)(20_000_000, 10_000_000) ++
      row(10_000_000)(20_000_000, 10_000_000)
    )

  test("uniform respects the aspect ratio (2:1)"):
    val offsets8 = OffsetGenerator.uniform(8.refined, offset(10_000_000L, 10_000_000L), offset(30_000_000L, 20_000_000L))
    assertEquals(
      offsets8.toList,
      row(20_000_000)(30_000_000, 23_333_334, 16_666_667, 10_000_000) ++
      row(10_000_000)(30_000_000, 23_333_334, 16_666_667, 10_000_000)
    )
    val offsets18 = OffsetGenerator.uniform(18.refined, offset(10_000_000L, 10_000_000L), offset(30_000_000L, 20_000_000L))
    assertEquals(
      offsets18.toList,
      row(20_000_000)(30_000_000, 26_000_000, 22_000_000, 18_000_000, 14_000_000, 10_000_000) ++
      row(15_000_000)(30_000_000, 26_000_000, 22_000_000, 18_000_000, 14_000_000, 10_000_000) ++
      row(10_000_000)(30_000_000, 26_000_000, 22_000_000, 18_000_000, 14_000_000, 10_000_000)
    )

  test("uniform respects the aspect ratio (almost 2:1)"):
    val offsets = OffsetGenerator.uniform(8.refined, offset(10_000_000L, 10_000_000L), offset(30_000_002L, 20_000_000L))
    assertEquals(
      offsets.toList,
      row(20_000_000)(30_000_002, 23_333_335, 16_666_668, 10_000_000) ++
      row(10_000_000)(30_000_002, 23_333_335, 16_666_668, 10_000_000)
    )

  test("grid generates correct number of offsets"):
    val offsets = OffsetGenerator.grid(3.refined, 4.refined, 1.arcsec, 2.arcsec)
    assertEquals(offsets.length, 12) // 3 * 4

  test("grid generates symmetric pattern around center"):
    val offsets = OffsetGenerator.grid(3.refined, 3.refined, 1.arcsec, 1.arcsec)
    assertEquals(offsets.length, 9)
    assert(offsets.toList.contains(Offset.Zero))
    val centerIndex = offsets.toList.indexOf(Offset.Zero)
    assert(centerIndex >= 0)

  test("grid handles single point"):
    val offsets = OffsetGenerator.grid(1.refined, 1.refined, 1.arcsec, 1.arcsec)
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
    val offsets = OffsetGenerator.grid(2.refined, 2.refined, -1.arcsec, -1.arcsec)
    assertEquals(offsets.length, 4)

  test("gridP generates P components"):
    val components = OffsetGenerator.gridP(5.refined, 1.arcsec)
    assertEquals(components.length, 5)
    // P components should be symmetric around center
    val pValues = components.toList.map(_.toAngle.toSignedDoubleRadians).sorted
    assertEquals(pValues.length, 5)

  test("gridQ generates Q components"):
    val components = OffsetGenerator.gridQ(3.refined, 2.arcsec)
    assertEquals(components.length, 3)
    // Should have center
    assert(components.toList.contains(Offset.Q.Zero))

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
