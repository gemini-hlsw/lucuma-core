// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.math.Angle
import lucuma.core.model.PosAngleConstraint
import lucuma.core.util.Enumerated
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ObservingModeType derives Enumerated:
  def tag: String
  def defaultPosAngleConstraint: PosAngleConstraint
  def observatory: Observatory

  def fold[A](
    fe: ExchangeObservingModeType => A,
    ff: FacilityObservingModeType => A,
    fv: VisitorObservingModeType  => A
  ): A =
    this match
      case m: ExchangeObservingModeType => fe(m)
      case m: FacilityObservingModeType => ff(m)
      case m: VisitorObservingModeType  => fv(m)

  def isExchange: Boolean =
    fold(_ => true, _ => false, _ => false)

  def isFacility: Boolean =
    fold(_ => false, _ => true, _ => false)

  def isVisitor: Boolean =
    fold(_ => false, _ => false, _ => true)

object ObservingModeType:
  export FacilityObservingModeType.{ values => _, * }
  export VisitorObservingModeType.{ values => _, * }
  export ExchangeObservingModeType.{ values => _, * }
  val values = FacilityObservingModeType.values ++ VisitorObservingModeType.values ++ ExchangeObservingModeType.values

  val toExchange: Prism[ObservingModeType, ExchangeObservingModeType] =
    GenPrism[ObservingModeType, ExchangeObservingModeType]

  val toFacility: Prism[ObservingModeType, FacilityObservingModeType] =
    GenPrism[ObservingModeType, FacilityObservingModeType]

  val toVisitor: Prism[ObservingModeType, VisitorObservingModeType] =
    GenPrism[ObservingModeType, VisitorObservingModeType]

// Exchange observing modes: a Gemini PI requesting time at another observatory
// (Keck or Subaru) via an exchange.  The mode tag encodes the observatory; the
// specific exchange instrument is carried alongside in the observation's config.
// These are not Gemini instruments and are not supported by ITC or AGS, so they
// have no `Instrument`.
enum ExchangeObservingModeType(
  val tag: String,
  val observatory: Observatory
) extends ObservingModeType derives Enumerated:
  val defaultPosAngleConstraint = PosAngleConstraint.Fixed(Angle.Angle0) // for all exchanges
  case ExchangeKeck   extends ExchangeObservingModeType("exchange_keck",   Observatory.Keck)
  case ExchangeSubaru extends ExchangeObservingModeType("exchange_subaru", Observatory.Subaru)

enum FacilityObservingModeType(
  val tag: String,
  val instrument: Instrument,
  val defaultPosAngleConstraint: PosAngleConstraint
) extends ObservingModeType derives Enumerated:
  case Flamingos2Imaging  extends FacilityObservingModeType("flamingos_2_imaging",   Instrument.Flamingos2, PosAngleConstraint.Unbounded)
  case Flamingos2LongSlit extends FacilityObservingModeType("flamingos_2_long_slit", Instrument.Flamingos2, PosAngleConstraint.AverageParallactic)
  case GhostIfu           extends FacilityObservingModeType("ghost_ifu",             Instrument.Ghost,      PosAngleConstraint.Fixed(Angle.Angle0))
  case GmosNorthImaging   extends FacilityObservingModeType("gmos_north_imaging",    Instrument.GmosNorth,  PosAngleConstraint.Unbounded)
  case GmosNorthLongSlit  extends FacilityObservingModeType("gmos_north_long_slit",  Instrument.GmosNorth,  PosAngleConstraint.AverageParallactic)
  case GmosNorthMos       extends FacilityObservingModeType("gmos_north_mos",        Instrument.GmosNorth,  PosAngleConstraint.Fixed(Angle.Angle0))
  case GmosSouthImaging   extends FacilityObservingModeType("gmos_south_imaging",    Instrument.GmosSouth,  PosAngleConstraint.Unbounded)
  case GmosSouthLongSlit  extends FacilityObservingModeType("gmos_south_long_slit",  Instrument.GmosSouth,  PosAngleConstraint.AverageParallactic)
  case GmosSouthMos       extends FacilityObservingModeType("gmos_south_mos",        Instrument.GmosSouth,  PosAngleConstraint.Fixed(Angle.Angle0))
  case GnirsImaging       extends FacilityObservingModeType("gnirs_imaging",         Instrument.Gnirs,      PosAngleConstraint.Unbounded)
  case GnirsLongSlit      extends FacilityObservingModeType("gnirs_long_slit",       Instrument.Gnirs,      PosAngleConstraint.AverageParallactic)
  case GnirsIfu           extends FacilityObservingModeType("gnirs_ifu",             Instrument.Gnirs,      PosAngleConstraint.AverageParallactic)
  case Igrins2LongSlit    extends FacilityObservingModeType("igrins_2_long_slit",    Instrument.Igrins2,    PosAngleConstraint.AverageParallactic)

  override def observatory: Observatory =
    Observatory.Gemini

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

  override def observatory: Observatory =
    Observatory.Gemini
