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


  def fromTarget(target: Target): ObjectTracking = target match
    case t: Target.Sidereal => SiderealObjectTracking(t.tracking)
    case _                  => sys.error("Only sidereal targets supported")

  def fromAsterism(targets: NonEmptyList[Target]): ObjectTracking = 
    val trackingList = targets.toList.traverse(Target.sidereal.getOption).flatMap(NonEmptyList.fromList)
    trackingList.fold(sys.error("Only sidereal targets supported")) { sidereals =>
      if (sidereals.length === 1) SiderealObjectTracking(sidereals.head.tracking)
      else SiderealAsterismTracking(sidereals.map(_.tracking))
    }

  def constant(coordinates: Coordinates): ObjectTracking =
    ConstantTracking(coordinates)
