// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import cats.kernel.Order
import cats.syntax.all.*
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.model.Configuration.ObservingMode.GmosNorthLongSlit
import lucuma.core.model.Configuration.ObservingMode.GmosSouthLongSlit

case class Configuration(conditions: Configuration.Conditions, target: Either[Coordinates, Region], observingMode: Configuration.ObservingMode) derives Eq:
  def subsumes(other: Configuration): Boolean =
    conditions >= other.conditions &&
    observingMode === other.observingMode && {
    (target, other.target) match
      case (Left(self), Left(other))   => observingMode.fov.toDoubleDegrees / 2.0 >= self.angularDistance(other).toDoubleDegrees
      case (Left(self), Right(other))  => false // coords never subsume region
      case (Right(self), Left(other))  => self.contains(other) // region contains coords
      case (Right(self), Right(other)) => self.containsAll(other) // region contains smaller region
    }

object Configuration:

  case class Conditions(
    cloudExtinction: CloudExtinction.Preset,
    imageQuality: ImageQuality.Preset,
    skyBackground: SkyBackground,
    waterVapor: WaterVapor,
  )

  object Conditions:

    given Order[Conditions] = 
      Order.reverse: // larger means better here
        Order.by: conds =>
          (conds.cloudExtinction, conds.imageQuality, conds.skyBackground, conds.waterVapor)

  // For now we define field of view as a disc of some angular radius on the sky.
  sealed abstract class ObservingMode(val tpe: ObservingModeType, val fov: Angle):
    def gmosNorthLongSlit: Option[GmosNorthLongSlit] = Some(this).collect { case m: GmosNorthLongSlit => m }
    def gmosSouthLongSlit: Option[GmosSouthLongSlit] = Some(this).collect { case m: GmosSouthLongSlit => m }

  object ObservingMode:

    // TODO: right now we're allowing sufficient slop to allow adjusting the target along [half] the length of the slit, but
    // this also allows moving to a target that's off the slit entirely since we're just measuring a single offset distance.
    // What we really need here is a polygon, but that's complicated by position angle (which may be allowed to flip or may
    // be chosen dynamically based on parallactic angle). 
    case class GmosNorthLongSlit(grating: GmosNorthGrating) extends ObservingMode(lucuma.core.enums.ObservingModeType.GmosNorthLongSlit, Angle.fromDoubleArcseconds(5.5 * 60)) // slit length of 5.5’
    case class GmosSouthLongSlit(grating: GmosSouthGrating) extends ObservingMode(lucuma.core.enums.ObservingModeType.GmosSouthLongSlit, Angle.fromDoubleArcseconds(5.5 * 60)) // slit length of 5.5’

    given Eq[ObservingMode] =
      Eq.instance:
        case (GmosNorthLongSlit(g1), GmosNorthLongSlit(g2)) => g1 === g2
        case (GmosSouthLongSlit(g1), GmosSouthLongSlit(g2)) => g1 === g2
        case _ => false
