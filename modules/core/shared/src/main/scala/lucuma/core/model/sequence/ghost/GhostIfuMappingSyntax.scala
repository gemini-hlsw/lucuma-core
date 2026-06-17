// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.geom.ghost.*
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.Target
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.util.Timestamp

/**
 * Syntax for the GhostIfuMapping companion that adds derivation and validation
 * from observation information.  Particularly, the resolution mode, the
 * (possible) sky position, the position angle, the observation time, and the
 * targets.
 */
object GhostIfuMappingSyntax:

  private enum AssignmentResult:
    case TargetAtIfu1 extends AssignmentResult
    case TargetAtIfu2 extends AssignmentResult
    case OutOfRange   extends AssignmentResult
    case TooClose     extends AssignmentResult

  import AssignmentResult.*

  private def ifuAssignment(
    base:          Coordinates,
    target:        Coordinates,
    skyOrTarget:   Option[Coordinates],
    positionAngle: Option[Angle]
  ): AssignmentResult =
    val angle  = positionAngle.getOrElse(Angle.Angle0)

    val field1 = GhostIfuPatrolField.ifu1PatrolFieldAt(angle, Offset.Zero)
    val field2 = GhostIfuPatrolField.ifu2PatrolFieldAt(angle, Offset.Zero)

    val shape1 = field1.eval
    val shape2 = field2.eval

    val pos1 = base.diff(target).offset
    val pos2 = skyOrTarget.map(base.diff(_).offset)

    val preliminaryResult =
      if shape1.contains(pos1) && pos2.forall(shape2.contains)      then TargetAtIfu1
      else if pos2.forall(shape1.contains) && shape2.contains(pos1) then TargetAtIfu2
      else OutOfRange

    extension (a: Angle)
      def µas: Long =
        Angle.signedMicroarcseconds.get(a).abs

    // Check whether the positions are too close.
    pos2.fold(preliminaryResult): p =>
      if pos1.distance(p).µas >= MinimumIfuArmSeparation.µas then preliminaryResult
      else TooClose

  private val MinimumArmSeparationString: String =
    Angle
      .signedDecimalArcseconds
      .get(MinimumIfuArmSeparation)
      .underlying
      .stripTrailingZeros
      .toPlainString

  private def deriveOneTarget(
    ctx:    IfuMappingContext,
    target: (Target.Id, Target)
  ): Either[String, GhostIfuMapping] =
    ctx.resolutionMode match
      case GhostResolutionMode.Standard =>
        target._2 match
          case Target.Sidereal(_, track, _, _) =>
            track.at(ctx.when.toInstant).fold("Cannot determine the target coordinates.".asLeft): c =>
              ifuAssignment(ctx.explicitBase.getOrElse(c), c, ctx.sky, ctx.angle) match
                case TargetAtIfu1 => ctx.sky.fold(GhostIfuMapping.SingleTarget(target._1).asRight)(s => GhostIfuMapping.TargetPlusSky(target._1, s).asRight)
                case TargetAtIfu2 => ctx.sky.fold("The target does not fall in range of GHOST IFU1 probe.".asLeft)(s => GhostIfuMapping.SkyPlusTarget(s, target._1).asRight)
                case OutOfRange   => "The target and sky positions are too far apart.".asLeft
                case TooClose     => s"The target and sky positions are too close (minimum separation is $MinimumArmSeparationString arcseconds).".asLeft

          case Target.Nonsidereal(_, _, _)     =>
            ctx.sky.fold(GhostIfuMapping.SingleTarget(target._1).asRight): _ =>
              "GHOST does not support sky positions for nonsidereal targets.".asLeft

          case Target.Opportunity(_, _, _)     =>
            "A GHOST IFU mapping can only be determined after the science target is identified.".asLeft

      case GhostResolutionMode.High =>
        target._2 match
          case Target.Sidereal(_, track, _, _) =>
            track.at(ctx.when.toInstant).fold("Cannot determine the target coordinates.".asLeft): c =>
              ctx.sky.fold("GHOST High Resolution mode requires a sky position.".asLeft): s =>
                ifuAssignment(ctx.explicitBase.getOrElse(c), c, s.some, ctx.angle) match
                  case TargetAtIfu1 => GhostIfuMapping.TargetPlusSky(target._1, s).asRight
                  case TooClose     => s"The target and sky positions are too close (minimum separation is $MinimumArmSeparationString arcseconds).".asLeft
                  case _            => "The target and/or sky position is not reachable by the GHOST IFU probes.".asLeft

          case Target.Nonsidereal(_, _, _)     =>
            ctx.sky.fold(GhostIfuMapping.SingleTarget(target._1).asRight): _ =>
              "GHOST does not support sky positions for nonsidereal targets.".asLeft

          case Target.Opportunity(_, _, _)     =>
            "A GHOST IFU mapping can only be determined after the science target is identified.".asLeft

  private def deriveDualTarget(
    ctx:     IfuMappingContext,
    targetA: (Target.Id, Target),
    targetB: (Target.Id, Target)
  ): Either[String, GhostIfuMapping] =
    (ctx.resolutionMode, ctx.sky) match
      case (GhostResolutionMode.Standard, None)    =>
        (targetA._2, targetB._2) match
          case (Target.Sidereal(_, track1, _, _), Target.Sidereal(_, track2, _, _)) =>
            (track1.at(ctx.when.toInstant), track2.at(ctx.when.toInstant))
              .tupled
              .fold("Cannot determine the target coordinates.".asLeft): (c1, c2) =>
                val base = ctx.explicitBase.getOrElse(Coordinates.centerOf(NonEmptyList.of(c1, c2)))
                ifuAssignment(base, c1, c2.some, ctx.angle) match
                  case TargetAtIfu1 => GhostIfuMapping.DualTarget(targetA._1, targetB._1).asRight
                  case TargetAtIfu2 => GhostIfuMapping.DualTarget(targetB._1, targetA._1).asRight
                  case OutOfRange   => "The targets do not fall in range of the GHOST IFU probes.".asLeft
                  case TooClose     => s"The dual targets are too close (minimum separation is $MinimumArmSeparationString arcseconds).".asLeft
          case _                                                                    =>
            "GHOST Dual Target mode is available for sidereal targets only.".asLeft

      case (GhostResolutionMode.Standard, Some(_)) =>
        "A sky position should not be defined for Dual Target mode.".asLeft

      case (GhostResolutionMode.High, _)           =>
        "Dual Target mode is only available in Standard Resolution.".asLeft


  extension (g: GhostIfuMapping.type)
    def derive(
      ctx:     IfuMappingContext,
      targets: List[(Target.Id, Target)]
    ): Either[String, GhostIfuMapping] =
      targets match
        case Nil          => "Cannot derive a GHOST IFU mapping until targets are defined.".asLeft
        case List(t)      => deriveOneTarget(ctx, t)
        case List(t1, t2) => deriveDualTarget(ctx, t1, t2)
        case _            => "Cannot derive a GHOST IFU mapping with more than two targets.".asLeft

    def validate(
      ctx:     IfuMappingContext,
      targets: List[(Target.Id, Target)]
    ): Option[String] =
      derive(ctx, targets).swap.toOption
