// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.math.Angle
import lucuma.core.model.PosAngleConstraint
import lucuma.core.util.Enumerated

sealed trait ObservingModeType derives Enumerated:
  def tag: String
  def instrument: Instrument
  def defaultPosAngleConstraint: PosAngleConstraint

object ObservingModeType:
  export FacilityObservingModeType.{ values => _, * }
  export VisitorObservingModeType.{ values => _, * }
  val values = FacilityObservingModeType.values ++ VisitorObservingModeType.values
    
enum FacilityObservingModeType(
  val tag: String, 
  val instrument: Instrument, 
  val defaultPosAngleConstraint: PosAngleConstraint
) extends ObservingModeType derives Enumerated:
  case Flamingos2LongSlit extends FacilityObservingModeType("flamingos_2_long_slit", Instrument.Flamingos2, PosAngleConstraint.AverageParallactic)
  case GhostIfu           extends FacilityObservingModeType("ghost_ifu",             Instrument.Ghost,      PosAngleConstraint.Fixed(Angle.Angle0))
  case GmosNorthImaging   extends FacilityObservingModeType("gmos_north_imaging",    Instrument.GmosNorth,  PosAngleConstraint.Unbounded)
  case GmosNorthLongSlit  extends FacilityObservingModeType("gmos_north_long_slit",  Instrument.GmosNorth,  PosAngleConstraint.AverageParallactic)
  case GmosSouthImaging   extends FacilityObservingModeType("gmos_south_imaging",    Instrument.GmosSouth,  PosAngleConstraint.Unbounded)
  case GmosSouthLongSlit  extends FacilityObservingModeType("gmos_south_long_slit",  Instrument.GmosSouth,  PosAngleConstraint.AverageParallactic)
  case GnirsLongSlit      extends FacilityObservingModeType("gnirs_long_slit",       Instrument.Gnirs,      PosAngleConstraint.AverageParallactic)
  case Igrins2LongSlit    extends FacilityObservingModeType("igrins_2_long_slit",    Instrument.Igrins2,    PosAngleConstraint.AverageParallactic)

enum VisitorObservingModeType(
  val tag: String, 
  val instrument: Instrument
) extends ObservingModeType derives Enumerated:
  val defaultPosAngleConstraint = PosAngleConstraint.Fixed(Angle.Angle0) // for all visitors
  case AlopekeSpeckle     extends VisitorObservingModeType("alopeke_speckle",        Instrument.Alopeke)
  case AlopekeWideField   extends VisitorObservingModeType("alopeke_wide_field",     Instrument.Alopeke)
  case VisitorNorth       extends VisitorObservingModeType("visitor_north",          Instrument.VisitorNorth)
  case VisitorSouth       extends VisitorObservingModeType("visitor_south",          Instrument.VisitorSouth)
  case MaroonX            extends VisitorObservingModeType("maroon_x",               Instrument.MaroonX)
  case ZorroSpeckle       extends VisitorObservingModeType("zorro_speckle",          Instrument.Zorro)
  case ZorroWideField     extends VisitorObservingModeType("zorro_wide_field",       Instrument.Zorro)

