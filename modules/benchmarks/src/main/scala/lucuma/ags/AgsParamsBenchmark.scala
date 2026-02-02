// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package lucuma.ags

// import cats.data.NonEmptyList
// import lucuma.core.enums.GmosNorthFpu
// import lucuma.core.enums.PortDisposition
// import lucuma.core.geom._
// import lucuma.core.math.Angle
// import lucuma.core.math.Offset
// import org.openjdk.jmh.annotations._

// import java.util.concurrent.TimeUnit

// @State(Scope.Thread)
// @Fork(1)
// @BenchmarkMode(Array(Mode.Throughput))
// @OutputTimeUnit(TimeUnit.MILLISECONDS)
// @Warmup(iterations = 4, time = 1)
// @Measurement(iterations = 10, time = 10)
// class AgsParamsBenchmark:

//   val gmos: AgsParams.GmosAgsParams = AgsParams.GmosAgsParams(
//     fpu = Some(Left(GmosNorthFpu.LongSlit_0_50)),
//     port = PortDisposition.Side
//   )

//   val one: NonEmptyList[AgsPosition] = NonEmptyList.one(
//     AgsPosition(
//       geometryType = GeometryType.SciOffset,
//       posAngle = Angle.Angle0,
//       offsetPos = Offset.Zero
//     )
//   )

//   val positions: NonEmptyList[AgsPosition] = NonEmptyList.of(
//     AgsPosition(GeometryType.SciOffset, Angle.Angle0, Offset.Zero),
//     AgsPosition(GeometryType.SciOffset, Angle.fromDoubleDegrees(90), Offset.Zero),
//     AgsPosition(GeometryType.SciOffset,
//                 Angle.Angle0,
//                 Offset.signedDecimalArcseconds.reverseGet(10.0, 10.0)
//     ),
//     AgsPosition(GeometryType.SciOffset,
//                 Angle.fromDoubleDegrees(45),
//                 Offset.signedDecimalArcseconds.reverseGet(5.0, -5.0)
//     )
//   )

//   val guideStarOffset: Offset        = Offset.signedDecimalArcseconds.reverseGet(60.0, 30.0)
//   val anotherGuideStarOffset: Offset = Offset.signedDecimalArcseconds.reverseGet(120.0, 60.0)

//   lazy val singlePosCalc: AgsGeomCalc =
//     gmos.posCalculations(one).lookup(one.head).get

//   lazy val multiPosCalc: AgsGeomCalc =
//     gmos.posCalculations(positions).lookup(positions.head).get

//   @Benchmark
//   def posCalculationsSingle: Unit = {
//     gmos.posCalculations(one)
//     ()
//   }

//   @Benchmark
//   def posCalculationsMultiple: Unit = {
//     gmos.posCalculations(positions)
//     ()
//   }

//   @Benchmark
//   def isReachable: Boolean =
//     singlePosCalc.isReachable(guideStarOffset)

//   @Benchmark
//   def isReachableMultiple: Boolean =
//     multiPosCalc.isReachable(guideStarOffset) &&
//       multiPosCalc.isReachable(anotherGuideStarOffset)

//   @Benchmark
//   def vignettingArea: Area =
//     singlePosCalc.vignettingArea(guideStarOffset)

//   @Benchmark
//   def overlapsScience: Boolean =
//     singlePosCalc.overlapsScience(guideStarOffset)

//   @Benchmark
//   def intersectionPatrolField: ShapeExpression =
//     singlePosCalc.intersectionPatrolField
