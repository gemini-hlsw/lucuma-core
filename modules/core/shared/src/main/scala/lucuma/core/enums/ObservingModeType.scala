// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.math.Angle
import lucuma.core.model.PosAngleConstraint
import lucuma.core.util.Enumerated

enum ObservingModeType(val tag: String, val instrument: Instrument, val defaultPosAngleConstraint: PosAngleConstraint) derives Enumerated:
  case Flamingos2LongSlit extends ObservingModeType("flamingos_2_long_slit", Instrument.Flamingos2, PosAngleConstraint.AverageParallactic)
  case GhostIfu           extends ObservingModeType("ghost_ifu",             Instrument.Ghost,      PosAngleConstraint.Fixed(Angle.Angle0))
  case GmosNorthImaging   extends ObservingModeType("gmos_north_imaging",    Instrument.GmosNorth,  PosAngleConstraint.Unbounded)
  case GmosNorthLongSlit  extends ObservingModeType("gmos_north_long_slit",  Instrument.GmosNorth,  PosAngleConstraint.AverageParallactic)
  case GmosSouthImaging   extends ObservingModeType("gmos_south_imaging",    Instrument.GmosSouth,  PosAngleConstraint.Unbounded)
  case GmosSouthLongSlit  extends ObservingModeType("gmos_south_long_slit",  Instrument.GmosSouth,  PosAngleConstraint.AverageParallactic)
  case Igrins2LongSlit    extends ObservingModeType("igrins_2_long_slit",    Instrument.Igrins2,    PosAngleConstraint.AverageParallactic)