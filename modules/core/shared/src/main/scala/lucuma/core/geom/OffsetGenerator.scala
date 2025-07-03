// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.Offset

import scala.math.Pi
import scala.math.ceil
import scala.math.sqrt
import scala.util.Random

/**
 * Based on the Python offset-generator script.
 */
object OffsetGenerator:

  /**
   * Generates a grid pattern of offsets.
   */
  def grid(nx: PosInt, ny: PosInt, dp: Angle, dq: Angle, center: Offset = Offset.Zero): NonEmptyList[Offset] =
    val pStart = -(nx.value - 1) / 2.0
    val qStart = (ny.value - 1) / 2.0

    val offsets =
      for
        j <- 0 until ny.value
        i <- 0 until nx.value
      yield
        val pOffset = dp * (pStart + i)
        val qOffset = dq * (qStart - j)
        center + Offset(pOffset.p, qOffset.q)

    // can't be empty as nx and ny are 1 or more
    NonEmptyList.fromListUnsafe(offsets.toList)

  /**
   * Generates a random pattern of offsets.
   * Creates a grid and randomly places points near each grid element.
   */
  def random(num: PosInt, size: Angle, center: Offset = Offset.Zero, random: Random = new Random()): NonEmptyList[Offset] =
    val n = PosInt.unsafeFrom(ceil(sqrt(num.value.toDouble)).toInt)
    val d = size * (1.0 / n.value.toDouble)

    val gridPoints = grid(n, n, d, d, center)

    val randomizedPoints = gridPoints.map: point =>
      val pJitter = d * (random.nextDouble() - 0.5)
      val qJitter = d * (random.nextDouble() - 0.5)
      point + Offset(pJitter.p, qJitter.q)

    // shuffle and take the requested number
    val shuffled = random.shuffle(randomizedPoints.toList)
    NonEmptyList.fromListUnsafe(shuffled.take(num.value))

  /**
   * Generates a Fermat spiral pattern of offsets.
   * Uses the golden angle for optimal packing.
   */
  def spiral(num: PosInt, size: Angle, center: Offset = Offset.Zero, random: Random = new Random()): NonEmptyList[Offset] =
    val θ = Angle.fromDoubleDegrees(137.50776)  // golden angle
    val φ = Angle.fromDoubleRadians(random.nextDouble() * 2.0 * Pi)

    // Calculate scale factor
    val arcsecs = Angle.signedDecimalArcseconds.get(size).toDouble
    val θʹ = θ * num.value.toDouble
    val α = arcsecs / 2.0 / sqrt(num.value * (θʹ.cos * θʹ.cos + θʹ.sin * θʹ.sin))

    val offsets =
      (0 until num.value).map: n =>
        val sqrtN = sqrt(n.toDouble)
        val angle = θ * n.toDouble + φ
        val p = α * sqrtN * angle.cos
        val q = α * sqrtN * angle.sin

        center + Offset(Angle.fromDoubleArcseconds(p).p, Angle.fromDoubleArcseconds(q).q)

    NonEmptyList.fromListUnsafe(offsets.toList)

  /**
   * Generates a 1D grid pattern of P components.
   */
  def gridP(num: PosInt, step: Angle, center: Offset.P = Offset.P.Zero): NonEmptyList[Offset.P] =
    val start = -(num.value - 1) / 2.0

    val components =
      (0 until num.value).map: i =>
        val pOffset = step * (start + i)
        center + Offset.P(pOffset)

    NonEmptyList.fromListUnsafe(components.toList)

  /**
   * Generates a 1D grid pattern of Q components.
   */
  def gridQ(num: PosInt, step: Angle, center: Offset.Q = Offset.Q.Zero): NonEmptyList[Offset.Q] =
    val start = (num.value - 1) / 2.0

    val components =
      (0 until num.value).map: i =>
        val qOffset = step * (start - i)
        center + Offset.Q(qOffset)

    NonEmptyList.fromListUnsafe(components.toList)

  /**
   * Generates a 1D random pattern of P components.
   */
  def randomP(num: PosInt, size: Angle, center: Offset.P = Offset.P.Zero, random: Random = new Random()): NonEmptyList[Offset.P] =
    val step = size * (1.0 / num.value.toDouble)
    val gridPoints = gridP(num, step, center)

    val randomizedPoints = gridPoints.map: point =>
      val pJitter = Offset.P(step * (random.nextDouble() - 0.5))
      point + pJitter

    val shuffled = random.shuffle(randomizedPoints.toList)
    NonEmptyList.fromListUnsafe(shuffled)

  /**
   * Generates a 1D random pattern of Q components.
   */
  def randomQ(num: PosInt, size: Angle, center: Offset.Q = Offset.Q.Zero, random: Random = new Random()): NonEmptyList[Offset.Q] =
    val step = size * (1.0 / num.value.toDouble)
    val gridPoints = gridQ(num, step, center)

    val randomizedPoints = gridPoints.map: point =>
      val qJitter = Offset.Q(step * (random.nextDouble() - 0.5))
      point + qJitter

    val shuffled = random.shuffle(randomizedPoints.toList)
    NonEmptyList.fromListUnsafe(shuffled)
