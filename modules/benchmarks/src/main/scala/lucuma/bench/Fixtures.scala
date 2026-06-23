// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

import cats.effect.IO
import cats.effect.std.Random as CatsRandom
import cats.effect.unsafe.implicits.global
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.OffsetGenerator
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.flamingos2
import lucuma.core.geom.gmos
import lucuma.core.geom.jts.JtsShape
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.jts.syntax.all.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.Geometry

import java.util.Random

/**
 * Define some AGS geometries to exercise jts and compare pre/post relateng
 */
object Fixtures:

  val posAngle: Angle                             = Angle.fromDoubleDegrees(45.0)
  val offset: Offset                              = Offset.Zero
  val port: PortDisposition                       = PortDisposition.Side
  val gmosFpu: Either[GmosNorthFpu, GmosSouthFpu] =
    Left(GmosNorthFpu.LongSlit_1_00)
  val f2Lyot: Flamingos2LyotWheel                 = Flamingos2LyotWheel.F16

  def geom(e: ShapeExpression): Geometry =
    e.eval match
      case JtsShape(g) => g
      case other       => sys.error(s"Expected JtsShape, got $other")

  // GMOS OIWFS

  val gmosPatrolFieldExpr: ShapeExpression =
    gmos.oiwfs.patrolField.longSlitMode.patrolFieldAt(posAngle, offset, gmosFpu, port)
  val gmosPatrolFieldShape: Shape          = gmosPatrolFieldExpr.eval
  val gmosPatrolFieldGeom: Geometry        = geom(gmosPatrolFieldExpr)

  def gmosProbeArmExpr(guideStar: Offset): ShapeExpression =
    gmos.oiwfs.probeArm.longSlit.shapeAt(posAngle, guideStar, offset, gmosFpu, port)

  // F2 OIWFS

  val f2PatrolFieldExpr: ShapeExpression =
    flamingos2.oiwfs.patrolField.patrolFieldAt(posAngle, offset, f2Lyot, port)
  val f2PatrolFieldShape: Shape          = f2PatrolFieldExpr.eval
  val f2PatrolFieldGeom: Geometry        = geom(f2PatrolFieldExpr)

  // Science target

  val scienceRadius: Angle               = Angle.fromDoubleArcseconds(50.0)
  val scienceTargetExpr: ShapeExpression =
    ShapeExpression.centeredEllipse(scienceRadius, scienceRadius) ↗ offset ⟲ posAngle
  val scienceTargetShape: Shape          = scienceTargetExpr.eval
  val scienceTargetGeom: Geometry        = geom(scienceTargetExpr)

  // GMOS imaging with many offsets

  val gmosImagingFovSize: Angle = Angle.fromMicroarcseconds(330340000L)

  // generate offsets in an spiral for gmos wita fixed seed
  def spiralOffsets(n: Int, size: Angle, seed: Int): List[Offset] =
    CatsRandom
      .scalaUtilRandomSeedInt[IO](seed)
      .flatMap: rnd =>
        given CatsRandom[IO] = rnd
        OffsetGenerator.Spiral(size).generate[IO](PosInt.unsafeFrom(n))
      .unsafeRunSync()
      .toList

  /**
   * Reduction of the GMOS imaging OIWFS patrol field over every offset with `∩`
   */
  def gmosImagingPatrolIntersectionExpr(offsets: List[Offset]): ShapeExpression =
    offsets
      .map(o => gmos.oiwfs.patrolField.imagingMode.patrolFieldAt(posAngle, o, port))
      .reduce(_ ∩ _)

  // Candidate guide stars
  def testGuideStarOffsets(shape: Shape, n: Int, seed: Long): Array[Offset] =
    // calculate positions on mas
    val b   = shape.boundingOffsets
    val pLo = Angle.signedMicroarcseconds.get(b.bottomRight.p.toAngle)
    val pHi = Angle.signedMicroarcseconds.get(b.topLeft.p.toAngle)
    val qLo = Angle.signedMicroarcseconds.get(b.bottomRight.q.toAngle)
    val qHi = Angle.signedMicroarcseconds.get(b.topLeft.q.toAngle)
    val rnd = new Random(seed)
    Array.fill(n):
      val p = pLo + (rnd.nextDouble() * (pHi - pLo)).toLong
      val q = qLo + (rnd.nextDouble() * (qHi - qLo)).toLong
      Offset(Angle.fromMicroarcseconds(p).p, Angle.fromMicroarcseconds(q).q)

  def offsetsToPoints(os: Array[Offset]): Array[Geometry] =
    os.map(_.point)

  def offsetsToCoords(os: Array[Offset]): Array[Coordinate] =
    os.map(_.coordinate)
