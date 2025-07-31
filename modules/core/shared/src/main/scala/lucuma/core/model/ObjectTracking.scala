// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.math.Coordinates
import lucuma.core.model.syntax.tracking.*
import lucuma.core.util.NewType

import java.time.Instant
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Opportunity
import lucuma.core.math.Region

// Tag to indicate the coordinates have been corrected for proper motion
object CoordinatesAtVizTime extends NewType[Coordinates]
type CoordinatesAtVizTime = CoordinatesAtVizTime.Type

/**
 * Generic representation to track an object. It is generalization of SiderealTracking but allows
 * tracking "virtual" objects like the center of an asterism
 */
sealed trait ObjectTracking derives Eq:
  def at(i: Instant): Option[CoordinatesAtVizTime]
  def baseCoordinates: Coordinates

object ObjectTracking:

  case class SiderealObjectTracking(tracking: SiderealTracking) extends ObjectTracking derives Eq:
    def at(i: Instant): Option[CoordinatesAtVizTime] =
      tracking.at(i).map(CoordinatesAtVizTime(_))
    def baseCoordinates: Coordinates                 = tracking.baseCoordinates

  case class SiderealAsterismTracking(trackings : NonEmptyList[SiderealTracking]) extends ObjectTracking derives Eq:
    def at(i: Instant): Option[CoordinatesAtVizTime] =
      trackings.centerOfAt(i).map(CoordinatesAtVizTime(_))
    def baseCoordinates: Coordinates                 = trackings.centerOf

  case class ConstantTracking(coordinates: Coordinates) extends ObjectTracking derives Eq:
    def at(i: Instant): Option[CoordinatesAtVizTime] = CoordinatesAtVizTime(coordinates).some
    def baseCoordinates: Coordinates = coordinates

  def fromTarget(target: Target): Option[ObjectTracking] =
    orRegionFromTarget(target).left.toOption

  def orRegionFromTarget(target: Target): Either[ObjectTracking, Region] =
    target match
      case t: Target.Sidereal    => SiderealObjectTracking(t.tracking).asLeft
      case t: Target.Opportunity => t.region.asRight 
      case t: Target.Nonsidereal => sys.error("Nonsidereal targets not supported yet.")    

  def fromAsterism(targets: NonEmptyList[Target]): Option[ObjectTracking] =
    orRegionFromAsterism(targets).left.toOption

  def orRegionFromAsterism(targets: NonEmptyList[Target]): Either[ObjectTracking, Region] =
    targets
      .collect:
        case Target.Nonsidereal(_, _, _) => sys.error("Nonsidereal targets not supported yet.")
        case Target.Opportunity(_, r, _) => r
      .headOption // first region, if any
      .toRight:
        targets
          .map: t =>
            Target
              .sidereal
              .getOption(t)
              .getOrElse(sys.error("unpossible, list should only contain sidereal targets"))          
          match          
            case NonEmptyList(h, Nil) => SiderealObjectTracking(h.tracking)
            case NonEmptyList(h, t)   => SiderealAsterismTracking(NonEmptyList(h, t).map(_.tracking))

  def constant(coordinates: Coordinates): ObjectTracking =
    ConstantTracking(coordinates)
