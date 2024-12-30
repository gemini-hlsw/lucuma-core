// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.f2

import cats.syntax.all.*
import coulomb.*
import coulomb.conversion.UnitConversion
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import lucuma.core.enums.F2LyotWheel
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.*
import spire.std.bigDecimal.*

// import scala.language.implicitConversions

/**
  * F2 area that could be reachable by the patrol arm
  * https://www.gemini.edu/instrumentation/flamingos-2/capability
  */
trait F2PatrolField {
            // Although normally this function only returns flips and offsets, I'm going to do the scaling to the LyotWheel plate scale here
            // final Flamingos2 f2 = (Flamingos2) ctx.getInstrument();
            // final double plateScale = f2.getLyotWheel().getPlateScale();
            // final AffineTransform xform = AffineTransform.getScaleInstance(plateScale, plateScale);
            //
            // // Flip it for the port?
            // // Validate(ctx.getAoComponent() == null) -> getFlipConfig(false)
            // final boolean flip = f2.getFlipConfig(false);
            // if (flip) {
            //     //Flip in X only
            //     xform.concatenate(AffineTransform.getScaleInstance(-1.0, 1.0));
            // }
            //
            // final int sign = flip ? -1 : 1;
            // final double rotationAngleInRadians = f2.getRotationConfig(false).toRadians().getMagnitude();
            // final AffineTransform rotateForPortAndFlip = AffineTransform.getRotateInstance(rotationAngleInRadians * sign);
            // xform.concatenate(rotateForPortAndFlip);
            //
            // // Apply complete transform
            // return new Some<>(new PatrolField(xform.createTransformedShape(getPatrolField().getArea())));


  /**
    * GMOS area where the probe arm can reach centered at 0
    */
  def candidatesArea: ShapeExpression =
    // 4.9 arcmin radius
    // NOTE There is some debate on whether this should be 4.8
    ShapeExpression.centeredEllipse((4.9 * 60 * 2).toInt.arcsec,
                                    (4.9 * 60 * 2).toInt.arcsec
    )

  /**
    * GMOS area where the probe arm can reach centered with a given posAngle and offset
    */
  def candidatesAreaAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    candidatesArea ↗ offsetPos ⟲ posAngle

  /**
    * GMOS area reachable by the problem arm for a set of posAngles and offsets
    */
  def candidatesAreaAt(posAngles: List[Angle], offsetPositions: List[Offset]): ShapeExpression =
    (for {
      a <- posAngles
      o <- offsetPositions
    } yield candidatesAreaAt(a, o)).fold(ShapeExpression.Empty)(_ ∪ _)

    //
    // static{
    //     // define the "upper" and "lower" half-circles defining the patrol are
    //     // -- use full circle for upper smaller one (using only half circle for upper part of figure can
    //     // -- end in two disjoint areas due to calculation imprecisions, we have to make sure areas overlap
    //     // -- properly to yield the figure we want).
    //     Ellipse2D.Double upperPa = new Ellipse2D.Double(
    //             PICK_OFF_PIVOT_POINT - UPPER_PATROL_AREA_RADIUS, -UPPER_PATROL_AREA_RADIUS,
    //             UPPER_PATROL_AREA_RADIUS*2., UPPER_PATROL_AREA_RADIUS*2.);
    //     // -- and combine with lower half-circle of bigger one
    //     Arc2D.Double lowerPA = new Arc2D.Double(
    //             BASE_PIVOT_POINT - LOWER_PATROL_AREA_RADIUS, -LOWER_PATROL_AREA_RADIUS,
    //             LOWER_PATROL_AREA_RADIUS*2., LOWER_PATROL_AREA_RADIUS*2.,
    //             180, 180, Arc2D.CHORD);
    //
    //     // define the two bounding shapes (one circle and a box)
    //     Ellipse2D.Double ew = new Ellipse2D.Double(
    //             -ENTRANCE_WINDOW_RADIUS, -ENTRANCE_WINDOW_RADIUS,
    //             ENTRANCE_WINDOW_RADIUS*2., ENTRANCE_WINDOW_RADIUS*2.0);
    //
    //     Rectangle2D.Double paLimit = new Rectangle2D.Double(
    //             -ENTRANCE_WINDOW_RADIUS, -ENTRANCE_WINDOW_RADIUS,
    //             ENTRANCE_WINDOW_RADIUS+PATROL_AREA_HI_LIMIT, 2*ENTRANCE_WINDOW_RADIUS);
    //
    //     // combination of lower and upper patrol areas (two half-circles)...
    //     Area pfArea = new Area(upperPa);
    //     pfArea.add(new Area(lowerPA));
    //     // .. intersected with bounding areas gives the patrol field
    //     pfArea.intersect(new Area(ew));
    //     pfArea.intersect(new Area(paLimit));
    //
    //     // there we go...
    //     patrolField = new PatrolField(pfArea);
    // }

  /**
    * F2 patrol field shape centered at the base position.
    */
  def patrolField(plateScale: Quantity[BigDecimal, ArcSecondPerMillimeter]): ShapeExpression =
    // define the "upper" and "lower" half-circles defining the patrol are
    // -- use full circle for upper smaller one (using only half circle for upper part of figure can
    // -- end in two disjoint areas due to calculation imprecisions, we have to make sure areas overlap
    // -- properly to yield the figure we want).
    val Two = BigDecimal(2)
    println( (UpperPatrolAreaRadius).withPlateScale(plateScale).toAngle.toMicroarcseconds / 1e6)
    val pfOffset = (PickOffPivotPoint, UpperPatrolAreaRadius * Two).withPlateScale(plateScale)
    val upperPA = ShapeExpression.centeredEllipse(
                (UpperPatrolAreaRadius * Two).withPlateScale(plateScale).toAngle,
                (UpperPatrolAreaRadius * Two).withPlateScale(plateScale).toAngle)
                // (UpperPatrolAreaRadius*Two, UpperPatrolAreaRadius*Two).withPlateScale(plateScale))

    val lowerPA = ShapeExpression.ellipseAt(
                (BasePivotPoint - LowerPatrolAreaRadius, -LowerPatrolAreaRadius).withPlateScale(plateScale),
                (LowerPatrolAreaRadius*Two, LowerPatrolAreaRadius*Two).withPlateScale(plateScale))

    // val i: Int = EntranceWindowRadius*Two
    // define the two bounding shapes (one circle and a box)
    val ew = ShapeExpression.ellipseAt(
                (-EntranceWindowRadius, -EntranceWindowRadius).withPlateScale(plateScale),
                (EntranceWindowRadius*Two, EntranceWindowRadius*Two).withPlateScale(plateScale))
    val paLimit = ShapeExpression.rectangleAt(
                (-EntranceWindowRadius, -EntranceWindowRadius).withPlateScale(plateScale),
                (EntranceWindowRadius + PatrolAreaHiLimit, EntranceWindowRadius * Two).withPlateScale(plateScale))

    ((upperPA ∪ lowerPA) ∩ ew) ∩ paLimit
    println(pfOffset)
    upperPA ↗ Offset(pfOffset._1, pfOffset._2)


  /**
    * F2 patrol field shape, in context.
    *
    * @param posAngle position angle where positive is counterclockwise
    * @param offsetPos offset position from the base, if any
    * @param fpu focal plane unit, if any
    * @param port port disposition
    *
    * @return probe field shape rotated and offset
    */
  def patrolFieldAt(
    posAngle:  Angle,
    offsetPos: Offset,
    lyotWheel: F2LyotWheel,
    // fpu:       Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port:      PortDisposition
  ): ShapeExpression = {
    val plateScale = BigDecimal(lyotWheel.plateScale).withUnit[ArcSecondPerMillimeter]
    // val pf = patrolField ↗ (ifuOffset(fpu) - Offset(94950.mas.p, 89880.mas.q))
    val pf = patrolField(plateScale) // ↗ (ifuOffset(fpu) - Offset(94950.mas.p, 89880.mas.q))
    val s  = if (port === PortDisposition.Side) pf.flipQ else pf
    s ↗ offsetPos ⟲ posAngle
  }

}

object patrolField extends F2PatrolField
