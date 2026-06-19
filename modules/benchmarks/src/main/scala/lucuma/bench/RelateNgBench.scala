// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import lucuma.core.geom.jts.interpreter.given
import org.locationtech.jts.algorithm.locate.IndexedPointInAreaLocator
import org.locationtech.jts.geom.Geometry
import org.locationtech.jts.geom.Location

/**
 * Micro benchmark of ags running some common geometries. Intended to measure the performance of JTS
 *
 * Cases: * S1 polygon.contains(point) x N candidate-star * S2 probeArm.intersects(science)
 * vignetting * S3 probeArm.intersection(science).area patrol-field overlap area * S4 N-offset
 * patrol-field intersection the AGS slow path * S5 IndexedPointInAreaLocator the prepared
 * point-in-polygon ceiling
 */
object RelateNgBench:

  private val N             = 4000
  private val MaxGuideStars = 500

  def main(args: Array[String]): Unit =
    import Fixtures.*

    println("RelateNG AGS benchmark")
    println(s"N query points = $N, guide-star samples = $MaxGuideStars")

    // guide stars within each patrol field's bounding box
    val gmosOffsets = testGuideStarOffsets(gmosPatrolFieldShape, N, seed = 1L)
    val f2Offsets   = testGuideStarOffsets(f2PatrolFieldShape, N, seed = 2L)
    val gmosPoints  = offsetsToPoints(gmosOffsets)
    val gmosCoords  = offsetsToCoords(gmosOffsets)
    val f2Points    = offsetsToPoints(f2Offsets)

    // Reachable guide stars
    val reachable = gmosOffsets.filter(gmosPatrolFieldShape.contains).take(MaxGuideStars)
    val starCount = math.max(reachable.length, 1)
    val gmosArms  = reachable.map(gs => geom(gmosProbeArmExpr(gs)))

    // S1: stateless contains
    Bench.section("S1 polygon.contains(point)  (stateless)")
    println(cycle("S1 GMOS contains(point)", gmosPoints.length) { i =>
      if gmosPatrolFieldGeom.contains(gmosPoints(i)) then 1L else 0L
    })
    println(cycle("S1 Flamingos2 contains(point)", f2Points.length) { i =>
      if f2PatrolFieldGeom.contains(f2Points(i)) then 1L else 0L
    })

    // S2: probe arm intersects science target
    Bench.section("S2 probeArm.intersects(scienceTarget)")
    println(cycle("S2 GMOS probeArm intersects science", starCount) { i =>
      if gmosArms(i).intersects(scienceTargetGeom) then 1L else 0L
    })

    // S3: intersection area
    Bench.section("S3 probeArm.intersection(scienceTarget).getArea  (overlay)")
    println(cycle("S3 GMOS intersection().getArea", starCount) { i =>
      java.lang.Double.doubleToLongBits(gmosArms(i).intersection(scienceTargetGeom).getArea)
    })

    // S4: many-offset patrol-field intersection
    Bench.section("S4 GMOS imaging N-offset patrol-field intersection (build + eval)")
    val offsetCounts = List(10, 50, 100)

    offsetCounts.foreach: n =>
      val offs = spiralOffsets(n, gmosImagingFovSize, seed = 1)
      println(Bench.run(s"S4 build+eval intersection, $n offsets", warmup = 5, iterations = 30) {
        java.lang.Double.doubleToLongBits(geom(gmosImagingPatrolIntersectionExpr(offs)).getArea)
      })
    val intersection100Expr = gmosImagingPatrolIntersectionExpr(
      spiralOffsets(100, gmosImagingFovSize, seed = 1)
    )
    val intersection100Geom = geom(intersection100Expr)
    val intersectionStars   = offsetsToPoints(
      testGuideStarOffsets(intersection100Expr.eval, N, seed = 3L)
    )
    println(cycle("S4 contains(point) on 100-offset intersection", intersectionStars.length) { i =>
      if intersection100Geom.contains(intersectionStars(i)) then 1L else 0L
    })

    // S5: indexed locator
    Bench.section("S5  IndexedPointInAreaLocator ceiling  (build once, query many)")
    val locator = new IndexedPointInAreaLocator(gmosPatrolFieldGeom)
    println(cycle("S5 IndexedPointInAreaLocator.locate", gmosCoords.length) { i =>
      if locator.locate(gmosCoords(i)) == Location.INTERIOR then 1L else 0L
    })

  private def cycle(
    name:       String,
    size:       Int,
    warmup:     Int = 50_000,
    iterations: Int = 200_000
  )(body: Int => Long): Bench.Result =
    var i = 0
    Bench.run(name, warmup, iterations) {
      val r = body(i)
      i += 1
      if i >= size then i = 0
      r
    }
