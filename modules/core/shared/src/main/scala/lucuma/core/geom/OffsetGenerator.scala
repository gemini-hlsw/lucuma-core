// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

import cats.Eq
import cats.Monad
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.std.Random as CatsRandom
import cats.syntax.applicative.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.geom.OffsetGenerator as OffsetGeneratorImpl
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenLens
import monocle.macros.GenPrism

import scala.math.Pi
import scala.math.ceil
import scala.math.sqrt



sealed trait OffsetGenerator {
  def generate[F[_]: {Monad, CatsRandom as R}](count: PosInt): F[NonEmptyList[Offset]]
}

object OffsetGenerator {

  /**
   * Generates a random pattern of offsets.
   * Creates a grid and randomly places points near each grid element.
   */
  case class Random(size: Angle, center: Offset = Offset.Zero) extends OffsetGenerator derives Eq {
    /**
     * Generates a grid pattern of offsets.
     */
    private def grid(nx: PosInt, ny: PosInt, dp: Angle, dq: Angle, center: Offset): NonEmptyList[Offset] = {
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
    }

    def generate[F[_]: {Monad, CatsRandom as R}](count: PosInt): F[NonEmptyList[Offset]] = {
      val n           = PosInt.unsafeFrom(ceil(sqrt(count.value.toDouble)).toInt)
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
      yield NonEmptyList.fromListUnsafe(shuffled.take(count.value))
    }
  }


  /**
   * Generates a Fermat spiral pattern of offsets.
   * Uses the golden angle for optimal packing.
   */
  case class Spiral(size:   Angle, center: Offset = Offset.Zero) extends OffsetGenerator derives Eq {
    def generate[F[_]: {Monad, CatsRandom as R}](count: PosInt): F[NonEmptyList[Offset]] = {
      val θ = Angle.fromDoubleDegrees(137.50776)  // golden angle

      val offsets = R.nextDouble.map(x => Angle.fromDoubleRadians(x * 2.0 * Pi)).map: φ =>
        // Calculate scale factor
        val arcsecs = Angle.signedDecimalArcseconds.get(size).toDouble
        val θʹ = θ * count.value.toDouble
        val α = arcsecs / 2.0 / sqrt(count.value * (θʹ.cos * θʹ.cos + θʹ.sin * θʹ.sin))

        (0 until count.value).map: n =>
          val sqrtN = sqrt(n.toDouble)
          val angle = θ * n.toDouble + φ
          val p = α * sqrtN * angle.cos
          val q = α * sqrtN * angle.sin

          center + Offset(Angle.fromDoubleArcseconds(p).p, Angle.fromDoubleArcseconds(q).q)

      offsets.map: o =>
        NonEmptyList.fromListUnsafe(o.toList)
    }
  }

  /**
   * Attempts to spread offsets uniformly, roughly matching the aspect ratio of
   * the bounding region.
   *
   * @param cornerA one corner of the bounding region
   * @param cornerB other corner of the bounding region
   */
  case class Uniform(cornerA: Offset, cornerB: Offset) extends OffsetGenerator derives Eq {
    def generate[F[_]: {Monad, CatsRandom as R}](count: PosInt): F[NonEmptyList[Offset]] = {
      val w = (cornerA.p.toSignedDecimalArcseconds - cornerB.p.toSignedDecimalArcseconds).abs
      val h = (cornerA.q.toSignedDecimalArcseconds - cornerB.q.toSignedDecimalArcseconds).abs

      val (rows, cols) =
        if h <= 0.000001 then  // 1 microarcsecond, the min angle resolution
          (1, count.value)
        else
          val aspectRatio = w / h
          val cols0 = 1 max Math.sqrt(count.value * aspectRatio.doubleValue).round.toInt
          val rows  = 1 max (count.value.toDouble / cols0).ceil.toInt
          val cols  = (count.value.toDouble / rows).ceil.toInt
          (rows, cols)

      val stepP = if cols <= 2 then w else w / (cols - 1) // arcseconds
      val stepQ = if rows <= 2 then h else h / (rows - 1) // arcseconds

      val p0 = cornerA.p.toSignedDecimalArcseconds max cornerB.p.toSignedDecimalArcseconds  // p increases to the left
      val q0 = cornerA.q.toSignedDecimalArcseconds max cornerB.q.toSignedDecimalArcseconds  // q increases to the top
      val o  = Offset.signedDecimalArcseconds.reverseGet((p0, q0))

      val offsets =
        (0 until rows).toList.flatMap: r =>
          (0 until cols).toList.map: c =>
            o + Offset.signedDecimalArcseconds.reverseGet(-stepP * c, -stepQ * r)

      NonEmptyList.fromListUnsafe(offsets.take(count.value)).pure[F]
    }
  }

  object Uniform {
    val cornerA: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerA)
    val cornerB: Lens[Uniform, Offset] = GenLens[Uniform](_.cornerB)
  }

  val random: Prism[OffsetGenerator, Random] = GenPrism[OffsetGenerator, Random]
  val spiral: Prism[OffsetGenerator, Spiral] = GenPrism[OffsetGenerator, Spiral]
  val uniform: Prism[OffsetGenerator, Uniform]   = GenPrism[OffsetGenerator, Uniform]
  
  val cornerA: Optional[OffsetGenerator, Offset] = uniform.andThen(Uniform.cornerA)
  val cornerB: Optional[OffsetGenerator, Offset] = uniform.andThen(Uniform.cornerB)

  val size: Optional[OffsetGenerator, Angle] =
    Optional.apply[OffsetGenerator, Angle] {
      case Random(s, _) => s.some
      case Spiral(s, _) => s.some
      case _            => none
    } { s => o =>
      o match
        case Random(_, c) => Random(s, c)
        case Spiral(_, c) => Spiral(s, c)
        case i            => i
    }

  val center: Optional[OffsetGenerator, Offset] =
    Optional.apply[OffsetGenerator, Offset] {
      case Random(_, c) => c.some
      case Spiral(_, c) => c.some
      case _            => none
    } { c => o =>
      o match
        case Random(s, _) => Random(s, c)
        case Spiral(s, _) => Spiral(s, c)
        case i            => i
    }


  given Eq[OffsetGenerator] with
    def eqv(x: OffsetGenerator, y: OffsetGenerator): Boolean =
      (x, y) match
        case (a @ Random(_, _),  b @ Random(_, _))  => a === b
        case (a @ Spiral(_, _),  b @ Spiral(_, _))  => a === b
        case (a @ Uniform(_, _), b @ Uniform(_, _)) => a === b
        case _                                      => false

}











// /**
//  * Based on the Python offset-generator script.
//  */
// object OffsetGenerator:
//   /**
//    * Generates a 1D grid pattern of P components.
//    */
//   def gridP(num: PosInt, step: Angle, center: Offset.P = Offset.P.Zero): NonEmptyList[Offset.P] =
//     val start = -(num.value - 1) / 2.0

//     val components =
//       (0 until num.value).map: i =>
//         val pOffset = step * (start + i)
//         center + Offset.P(pOffset)

//     NonEmptyList.fromListUnsafe(components.toList)

//   /**
//    * Generates a 1D grid pattern of Q components.
//    */
//   def gridQ(num: PosInt, step: Angle, center: Offset.Q = Offset.Q.Zero): NonEmptyList[Offset.Q] =
//     val start = (num.value - 1) / 2.0

//     val components =
//       (0 until num.value).map: i =>
//         val qOffset = step * (start - i)
//         center + Offset.Q(qOffset)

//     NonEmptyList.fromListUnsafe(components.toList)

//   private def withJitter[F[_]: {Monad, CatsRandom as R}, T](
//     gridPoints: NonEmptyList[T],
//     jitterScale: Angle,
//     jitterFn: (T, Angle) => T
//   ): F[NonEmptyList[T]] =
//     gridPoints.traverse: point =>
//       R.nextDouble.map(x => jitterFn(point, jitterScale * (2.0 * x - 1.0)))

//   /**
//    * Generates a 1D random pattern of P components.
//    */
//   def randomP[F[_]: {Monad, CatsRandom as R}](num: PosInt, size: Angle, center: Offset.P = Offset.P.Zero): F[NonEmptyList[Offset.P]] =
//     val step = size * (1.0 / num.value.toDouble)
//     val jitterScale = step * 0.5
//     val gridPoints = gridP(num, step, center)

//     for
//       randomizedPoints <- withJitter(gridPoints, jitterScale, _ + Offset.P(_))
//       shuffled         <- R.shuffleList(randomizedPoints.toList)
//     yield NonEmptyList.fromListUnsafe(shuffled)

//   /**
//    * Generates a 1D random pattern of Q components.
//    */
//   def randomQ[F[_]: {Monad, CatsRandom as R}](num: PosInt, size: Angle, center: Offset.Q = Offset.Q.Zero): F[NonEmptyList[Offset.Q]] =
//     val step = size * (1.0 / num.value.toDouble)
//     val jitterScale = step * 0.5
//     val gridPoints = gridQ(num, step, center)

//     for
//       randomizedPoints <- withJitter(gridPoints, jitterScale, _ + Offset.Q(_))
//       shuffled         <- R.shuffleList(randomizedPoints.toList)
//     yield NonEmptyList.fromListUnsafe(shuffled)
