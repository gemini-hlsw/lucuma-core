// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Enumerated
import lucuma.refined.*

/**
 * Enumerated type for instruments.
 *
 * @group Enumerations
 */
enum Instrument(val tag: String, val shortName: String, val longName: String, val referenceName: NonEmptyString, val obsolete: Boolean) derives Enumerated:
  /** @group Constructors */ case AcqCam     extends Instrument("AcqCam", "AcqCam", "Acquisition Camera", "ACQCAM".refined[NonEmpty], false)
  /** @group Constructors */ case Bhros      extends Instrument("Bhros", "bHROS", "bHROS", "BHROS".refined[NonEmpty], true)
  /** @group Constructors */ case Flamingos2 extends Instrument("Flamingos2", "Flamingos2", "Flamingos 2", "F2".refined[NonEmpty], false)
  /** @group Constructors */ case Ghost      extends Instrument("Ghost", "GHOST", "GHOST", "GHOST".refined[NonEmpty], false)
  /** @group Constructors */ case GmosNorth  extends Instrument("GmosNorth", "GMOS-N", "GMOS North", "GMOSN".refined[NonEmpty], false)
  /** @group Constructors */ case GmosSouth  extends Instrument("GmosSouth", "GMOS-S", "GMOS South", "GMOSS".refined[NonEmpty], false)
  /** @group Constructors */ case Gnirs      extends Instrument("Gnirs", "GNIRS", "GNIRS", "GNIRS".refined[NonEmpty], false)
  /** @group Constructors */ case Gpi        extends Instrument("Gpi", "GPI", "GPI", "GPI".refined[NonEmpty], false)
  /** @group Constructors */ case Gsaoi      extends Instrument("Gsaoi", "GSAOI", "GSAOI", "GSAOI".refined[NonEmpty], false)
  /** @group Constructors */ case Igrins2    extends Instrument("Igrins2", "IGRINS2", "IGRINS2", "IGRINS2".refined[NonEmpty], false)
  /** @group Constructors */ case Michelle   extends Instrument("Michelle", "Michelle", "Michelle", "MICHELLE".refined[NonEmpty], false)
  /** @group Constructors */ case Nici       extends Instrument("Nici", "NICI", "NICI", "NICI".refined[NonEmpty], false)
  /** @group Constructors */ case Nifs       extends Instrument("Nifs", "NIFS", "NIFS", "NIFS".refined[NonEmpty], false)
  /** @group Constructors */ case Niri       extends Instrument("Niri", "NIRI", "NIRI", "NIRI".refined[NonEmpty], false)
  /** @group Constructors */ case Phoenix    extends Instrument("Phoenix", "Phoenix", "Phoenix", "PHOENIX".refined[NonEmpty], false)
  /** @group Constructors */ case Trecs      extends Instrument("Trecs", "TReCS", "TReCS", "TRECS".refined[NonEmpty], false)
  /** @group Constructors */ case Visitor    extends Instrument("Visitor", "Visitor Instrument", "Visitor Instrument", "VISITOR".refined[NonEmpty], false)
  /** @group Constructors */ case Scorpio    extends Instrument("Scorpio", "SCORPIO", "Scorpio", "SCORPIO".refined[NonEmpty], false)
  /** @group Constructors */ case Alopeke    extends Instrument("Alopeke", "ALOPEKE", "Alopeke", "ALOPEKE".refined[NonEmpty], false)
  /** @group Constructors */ case Zorro      extends Instrument("Zorro", "ZORRO", "Zorro", "ZORRO".refined[NonEmpty], false)
