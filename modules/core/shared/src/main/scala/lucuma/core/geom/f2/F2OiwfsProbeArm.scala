// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.f2

import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import spire.std.bigDecimal.*

import java.lang.Math.PI
import java.lang.Math.asin
import java.lang.Math.atan2
import java.lang.Math.hypot
import java.lang.Math.sin
import lucuma.core.enums.F2LyotWheel

/**
  * Description of the GMOS OIWFS probe arm geometry.
  */
trait F2OiwfsProbeArm {
    // val probeArm: Shape = {
    //   val plateScale = f2.getLyotWheel.getPlateScale
    //   val scaledLength = ProbePickoffArmLength * plateScale
    //   val hm = PickoffMirrorSize * plateScale / 2.0
    //   val htw = ProbeArmTaperedWidth / 2.0
    //
    //   val (x0, y0) = (hm, -htw)
    //   val (x1, y1) = (x0 + ProbeArmTaperedLength, -hm)
    //   val (x2, y2) = (x0 + scaledLength, y1)
    //   val (x3, y3) = (x2, hm)
    //   val (x4, y4) = (x1, y3)
    //   val (x5, y5) = (x0, htw)
    //
    //   val points = List((x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5))
    //   ImPolygon(points)
    // }
    //
    // val pickoffMirror: Shape = {
    //   val plateScale = f2.getLyotWheel.getPlateScale
    //   val scaledMirrorSize = PickoffMirrorSize * plateScale
    //   val xy = -scaledMirrorSize / 2.0
    //   new Rectangle2D.Double(xy, xy, scaledMirrorSize, scaledMirrorSize)
    // }
    //
    // new Area(probeArm) <| (_.add(new Area(pickoffMirror)))

  private def arm(plateScale: F2PlateScale): ShapeExpression = {
    val scaledLength = ProbePickoffArmLength.withPlateScale(plateScale).toAngle
    val hm = (PickoffMirrorSize.withPlateScale(plateScale) / 2.0).toAngle
    val htw = ProbeArmTaperedWidth.bisect

    val (x0, y0) = (hm, -htw)
    val (x1, y1) = (x0 + ProbeArmTaperedLength, -hm)
    val (x2, y2) = (x0 + scaledLength, y1)
    val (x3, y3) = (x2, hm)
    val (x4, y4) = (x1, y3)
    val (x5, y5) = (x0, htw)

    ShapeExpression.polygonAt((x0.p, y0.q), (x1.p, y1.q), (x2.p, y2.q), (x3.p, y3.q), (x4.p, y4.q), (x5.p, y5.q))
  }

  private def pickoff(plateScale: F2PlateScale): ShapeExpression = {
    val scaledMirrorSize = PickoffMirrorSize.withPlateScale(plateScale).toAngle
    val xy = -scaledMirrorSize.bisect
    ShapeExpression.centeredRectangle(scaledMirrorSize, scaledMirrorSize)
  }

  /**
    * Description of the F2 OIWFS probe arm with the pickoff mirror centered
    * at the base position.
    */
  def shape(plateScale: F2PlateScale): ShapeExpression =
    arm(plateScale) ∪ pickoff(plateScale)

  /**
    * The F2 OIWFS probe arm positioned to reach a particular guide star at
    * a particular offset.
    *
    * @param posAngle position angle where positive is counterclockwise
    * @param guideStar guide star offset from the center, relative to an un-rotated frame
    * @param offsetPos offset position from the base, if any
    * @param fpu focal plane unit, if any
    * @param port port disposition
    *
    * @return probe arm shape correctly rotated and offset to reach the guide star
    */
  def shapeAt(
    posAngle:  Angle,
    guideStar: Offset,
    offsetPos: Offset,
    lyot:      F2LyotWheel,
    port:      PortDisposition
  ): ShapeExpression =
    val plateScale = BigDecimal(lyot.plateScale).withUnit[ArcSecondPerMillimeter]
    println(s"Arm angle ${armAngle(posAngle, guideStar, offsetPos, port, plateScale).toDoubleDegrees}")
    shape(plateScale) ⟲ armAngle(posAngle, guideStar, offsetPos, port, plateScale) ↗ guideStar

  private def armAngle(posAngle:    Angle,
                       gsOffset:    Offset,
                       offset:      Offset,
                       port:        PortDisposition,
                       plateScale:  F2PlateScale): Angle = {
    val Q = {
      val P = {
        val scaledFlippedPAO = {
          val scaledPAO = ProbeArmOffset.withPlateScale(plateScale).value.toDouble
          println(s"PAO ${scaledPAO} $port ")
          if (port === PortDisposition.Bottom) -scaledPAO else scaledPAO
        }
        val angle = -posAngle
        // println(s"P ${angle.toDoubleDegrees} ${Angle.fromDoubleArcseconds(scaledFlippedPAO * angle.cos).toSignedDoubleDegrees}, ${Angle.fromDoubleArcseconds(scaledFlippedPAO * angle.sin).toSignedDoubleDegrees}")
        // println(s" --> ${(scaledFlippedPAO * angle.cos)}, ${(scaledFlippedPAO * angle.sin)}")
        ((scaledFlippedPAO * angle.cos).withUnit[ArcSecond],
         (scaledFlippedPAO * angle.sin).withUnit[ArcSecond])
      }

      val guideStar = (gsOffset - offset.rotate(posAngle)).inverse().toDoubleArcseconds
      println(s"P ${P._1} ${P._2}")
      // println(s"gs offset ${gsOffset.p.toAngle.toDoubleDegrees} ${gsOffset.q.toAngle.toDoubleDegrees}")
      println(s"Guide star $guideStar ${Angle.signedDecimalArcseconds.get(gsOffset.p.toAngle)} ${Angle.signedDecimalArcseconds.get(gsOffset.q.toAngle)}")
      // println(s"Guide star $guideStar ${Angle.signedDecimalArcseconds.get(-guideStar.p.toAngle)} ${Angle.signedDecimalArcseconds.get(-guideStar.q.toAngle)}")
      val D: (Quantity[BigDecimal, ArcSecond], Quantity[BigDecimal, ArcSecond]) = (guideStar._1 - P._1, guideStar._2 - P._2)
      val dOffset = Offset(Angle.fromDoubleArcseconds((guideStar._1 - P._1).value.toDouble).p, Angle.fromDoubleArcseconds((guideStar._2 - P._2).value.toDouble).q)
      // println(s"D ${(guideStar.p.toAngle.toMicroarcseconds / 1e6) - (P.p.toAngle.toMicroarcseconds / 1e6)} ${(guideStar.q.toAngle.toMicroarcseconds / 1e6) - (P.q.toAngle.toMicroarcseconds / 1e6)}")
      // println(s"D ${D.p.toAngle.toMicroarcseconds / 1e6} ${D.q.toAngle.toMicroarcseconds / 1e6}")
      println(s"offset ${offset} ")
      println(s"GS ${guideStar._1.value} ${guideStar._2.value}")
      println(s"D ${D._1.value} ${D._2.value}")

      val scaledPBAL = ProbeBaseArmLength.withPlateScale(plateScale).value.toDouble
      val scaledPPAL = ProbePickoffArmLength.withPlateScale(plateScale).value.toDouble
      val distance: Quantity[BigDecimal, ArcSecond]   = math.min(math.sqrt((D._1.value.pow(2) + D._2.value.pow(2)).toDouble), scaledPBAL + scaledPPAL).withUnit[ArcSecond]
      // val distance = dOffset.distance(Offset(Angle.signedDecimalArcseconds.reverseGet(scaledPBAL).p, Angle.signedDecimalArcseconds.reverseGet(scaledPPAL).q)).toMicroarcseconds
      println(s" scaledpal  ${scaledPBAL}")
      println(s" scaledppl  ${scaledPPAL}")
      println(s" Distance ${distance }")
      val a = (scaledPBAL * scaledPBAL - scaledPPAL * scaledPPAL + distance.value.pow(2)) / (2 * distance.value)
      println(s" a ${a}")
      val h = (if (port === PortDisposition.Bottom) -1 else 1) * math.sqrt((scaledPBAL * scaledPBAL - a * a).toDouble)
      println(s" h ${h}")
      val p1 = (a * D._1.value + h * D._2.value) / distance.value
      val q1 = (a * D._2.value - h * D._1.value) / distance.value
      println(p1)
      println(q1)
      // val q1 = Angle.fromMicroarcseconds(((a * D.q.toAngle.toMicroarcseconds - h * D.p.toAngle.toMicroarcseconds) / distance).toLong)
      val p = -guideStar._1.value + P._1.value + p1
      val q = -guideStar._2.value + P._2.value + q1
      (p, q)
      // Offset(Angle.signedDecimalArcseconds.reverseGet(p).p, Angle.signedDecimalArcseconds.reverseGet(q).q)
    }
    println(s"atan $Q ${math.atan2(Q._2.toDouble, Q._1.toDouble)}")
    val k = Angle.fromDoubleRadians(-math.atan2(Q._2.toDouble, Q._1.toDouble))
    println(k.toDoubleDegrees)
    k.flip
  }
  // private def armAngle(
  //   posAngle:  Angle,
  //   guideStar: Offset,
  //   offsetPos: Offset,
  //   fpu:       Option[Either[GmosNorthFpu, GmosSouthFpu]],
  //   port:      PortDisposition
  // ): Angle = {
  //
  //   val t  = Offset(427520.mas.p, 101840.mas.q)
  //   val tʹ =
  //     if (port === PortDisposition.Side) Offset.qAngle.modify(_.mirrorBy(Angle.Angle0))(t) else t
  //
  //   val bx  = Angle.signedDecimalArcseconds.get(StageArmLength).toDouble
  //   val bxᒾ = bx * bx
  //
  //   val mx  = Angle.signedDecimalArcseconds.get(PickoffArmLength).toDouble
  //   val mxᒾ = mx * mx
  //
  //   val (x, y) =
  //     Offset.signedDecimalArcseconds
  //       .get(
  //         tʹ.rotate(posAngle) + guideStar - (offsetPos - ifuOffset(fpu)).rotate(posAngle)
  //       )
  //       .bimap(x => -x.toDouble, _.toDouble)
  //
  //   val r  = hypot(x, y)
  //   val rᒾ = r * r
  //   val α  = atan2(x, -y)
  //
  //   val φ =
  //     math.acos(
  //       (rᒾ - (bxᒾ + mxᒾ)) / (2.0 * bx * mx) match {
  //         case a if a < -1.0 => -1.0
  //         case a if a > 1.0  => 1.0
  //         case a             => a
  //       }
  //     ) * (if (port === PortDisposition.Side) -1.0 else 1.0)
  //
  //   val θ = {
  //     val θʹ = asin((mx / r) * sin(φ))
  //     if (mxᒾ > (rᒾ + bxᒾ)) PI - θʹ else θʹ
  //   }
  //
  //   Angle.fromDoubleRadians(-φ + θ + α + PI / 2.0)
  // }

}

object probeArm extends F2OiwfsProbeArm
