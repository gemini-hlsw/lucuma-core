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
import lucuma.core.model.Configuration.ObservingMode.*
import lucuma.core.geom.*
import lucuma.core.math.Offset
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.GmosSouthFilter

case class Configuration(conditions: Configuration.Conditions, target: Either[Coordinates, Region], observingMode: Configuration.ObservingMode) derives Eq:
  def subsumes(other: Configuration): Boolean =
    conditions >= other.conditions &&
    observingMode.subsumes(other.observingMode) && {
    (target, other.target) match
      case (Left(self), Left(other))   => observingMode.radius.toDoubleDegrees >= self.angularDistance(other).toDoubleDegrees
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
  
  sealed abstract class ObservingMode(val tpe: ObservingModeType, val radius: Angle):
    def gmosNorthLongSlit:  Option[GmosNorthLongSlit ] = Some(this).collect { case m: GmosNorthLongSlit  => m }
    def gmosSouthLongSlit:  Option[GmosSouthLongSlit ] = Some(this).collect { case m: GmosSouthLongSlit  => m }
    def gmosNorthImaging:   Option[GmosNorthImaging  ] = Some(this).collect { case m: GmosNorthImaging   => m }
    def gmosSouthImaging:   Option[GmosSouthImaging  ] = Some(this).collect { case m: GmosSouthImaging   => m }
    def flamingos2LongSlit: Option[Flamingos2LongSlit] = Some(this).collect { case m: Flamingos2LongSlit => m }

    def subsumes(other: ObservingMode): Boolean =
      (this, other) match
        case (GmosNorthLongSlit(g1), GmosNorthLongSlit(g2))   => g1 === g2
        case (GmosSouthLongSlit(g1), GmosSouthLongSlit(g2))   => g1 === g2
        case (GmosNorthImaging(f1), GmosNorthImaging(f2))     => f2.forall(f1.contains)
        case (GmosSouthImaging(f1), GmosSouthImaging(f2))     => f2.forall(f1.contains)
        case (Flamingos2LongSlit(d1), Flamingos2LongSlit(d2)) => d1 === d2
        case _                                                => false

  object ObservingMode:

    object Radii:
      val GmosLongSlit = gmos.scienceArea.longSlitFov(Angle.fromMicroarcseconds(2L)).eval.radius // width doesn't matter but should be > 1 µas
      val GmosImaging  = gmos.scienceArea.imaging.eval.radius
      val Flamingos2LongSlit = flamingos2.scienceArea.shapeAt(Angle.Angle0, Offset.Zero, Flamingos2LyotWheel.F16, Flamingos2FpuMask.Imaging).eval.radius

    case class GmosNorthLongSlit(grating: GmosNorthGrating) extends ObservingMode(ObservingModeType.GmosNorthLongSlit, Radii.GmosLongSlit)
    case class GmosSouthLongSlit(grating: GmosSouthGrating) extends ObservingMode(ObservingModeType.GmosSouthLongSlit, Radii.GmosLongSlit) 
    case class GmosNorthImaging(filters: List[GmosNorthFilter]) extends ObservingMode(ObservingModeType.GmosNorthImaging, Radii.GmosImaging)
    case class GmosSouthImaging(filters: List[GmosSouthFilter]) extends ObservingMode(ObservingModeType.GmosSouthImaging, Radii.GmosImaging)
    case class Flamingos2LongSlit(disperser: Flamingos2Disperser) extends ObservingMode(ObservingModeType.Flamingos2LongSlit, Radii.Flamingos2LongSlit)
