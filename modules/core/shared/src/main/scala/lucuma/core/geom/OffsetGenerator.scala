// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.std.Random
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.Offset

import scala.math.Pi
import scala.math.ceil
import scala.math.sqrt

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
  def random[F[_]: {Monad, Random as R}](num: PosInt, size: Angle, center: Offset = Offset.Zero): F[NonEmptyList[Offset]] =
    val n           = PosInt.unsafeFrom(ceil(sqrt(num.value.toDouble)).toInt)
    val d           = size * (1.0 / n.value.toDouble)
    val jitterScale = d * 0.5
    val gridPoints  = grid(n, n, d, d, center)

    def randomized =
      gridPoints.traverse: point =>
        for
          pJitter <- R.nextDouble.map(x => jitterScale * (2.0 * x - 1.0))
          qJitter <- R.nextDouble.map(x => jitterScale * (2.0 * x - 1.0))
        yield point + Offset(pJitter.p, qJitter.q)

    for
      randomizedPoints <- randomized
      shuffled         <- R.shuffleList(randomizedPoints.toList)
    yield NonEmptyList.fromListUnsafe(shuffled.take(num.value))

  /**
   * Generates a Fermat spiral pattern of offsets.
   * Uses the golden angle for optimal packing.
   */
  def spiral[F[_]: {Monad, Random as R}](num: PosInt, size: Angle, center: Offset = Offset.Zero): F[NonEmptyList[Offset]] =
    val θ = Angle.fromDoubleDegrees(137.50776)  // golden angle

    val offsets = R.nextDouble.map(x => Angle.fromDoubleRadians(x * 2.0 * Pi)).map: φ =>
      // Calculate scale factor
      val arcsecs = Angle.signedDecimalArcseconds.get(size).toDouble
      val θʹ = θ * num.value.toDouble
      val α = arcsecs / 2.0 / sqrt(num.value * (θʹ.cos * θʹ.cos + θʹ.sin * θʹ.sin))

      (0 until num.value).map: n =>
        val sqrtN = sqrt(n.toDouble)
        val angle = θ * n.toDouble + φ
        val p = α * sqrtN * angle.cos
        val q = α * sqrtN * angle.sin

        center + Offset(Angle.fromDoubleArcseconds(p).p, Angle.fromDoubleArcseconds(q).q)

    offsets.map: o =>
      NonEmptyList.fromListUnsafe(o.toList)

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

  private def withJitter[F[_]: {Monad, Random as R}, T](
    gridPoints: NonEmptyList[T],
    jitterScale: Angle,
    jitterFn: (T, Angle) => T
  ): F[NonEmptyList[T]] =
    gridPoints.traverse: point =>
      R.nextDouble.map(x => jitterFn(point, jitterScale * (2.0 * x - 1.0)))

  /**
   * Generates a 1D random pattern of P components.
   */
  def randomP[F[_]: {Monad, Random as R}](num: PosInt, size: Angle, center: Offset.P = Offset.P.Zero): F[NonEmptyList[Offset.P]] =
    val step = size * (1.0 / num.value.toDouble)
    val jitterScale = step * 0.5
    val gridPoints = gridP(num, step, center)

    for
      randomizedPoints <- withJitter(gridPoints, jitterScale, _ + Offset.P(_))
      shuffled         <- R.shuffleList(randomizedPoints.toList)
    yield NonEmptyList.fromListUnsafe(shuffled)

  /**
   * Generates a 1D random pattern of Q components.
   */
  def randomQ[F[_]: {Monad, Random as R}](num: PosInt, size: Angle, center: Offset.Q = Offset.Q.Zero): F[NonEmptyList[Offset.Q]] =
    val step = size * (1.0 / num.value.toDouble)
    val jitterScale = step * 0.5
    val gridPoints = gridQ(num, step, center)

    for
      randomizedPoints <- withJitter(gridPoints, jitterScale, _ + Offset.Q(_))
      shuffled         <- R.shuffleList(randomizedPoints.toList)
    yield NonEmptyList.fromListUnsafe(shuffled)
