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
enum Instrument(val tag: String, val shortName: String, val longName: String, val referenceName: NonEmptyString, val site: Site, val obsolete: Boolean) derives Enumerated:
  case AcqCamGS   extends Instrument("AcqCamGS", "AcqCam", "Acquisition Camera", "ACQCAMGS".refined[NonEmpty], Site.GS, false)
  case AcqCamGN   extends Instrument("AcqCamGN", "AcqCam", "Acquisition Camera", "ACQCAMGN".refined[NonEmpty], Site.GN, false)
  case Flamingos2 extends Instrument("Flamingos2", "Flamingos2", "Flamingos 2", "F2".refined[NonEmpty], Site.GS, false)
  case Ghost      extends Instrument("Ghost", "GHOST", "GHOST", "GHOST".refined[NonEmpty], Site.GS, false)
  case GmosNorth  extends Instrument("GmosNorth", "GMOS-N", "GMOS North", "GMOSN".refined[NonEmpty], Site.GN, false)
  case GmosSouth  extends Instrument("GmosSouth", "GMOS-S", "GMOS South", "GMOSS".refined[NonEmpty], Site.GS, false)
  case Gnirs      extends Instrument("Gnirs", "GNIRS", "GNIRS", "GNIRS".refined[NonEmpty], Site.GN, false)
  case Gpi        extends Instrument("Gpi", "GPI", "GPI", "GPI".refined[NonEmpty], Site.GN, false)
  case Gsaoi      extends Instrument("Gsaoi", "GSAOI", "GSAOI", "GSAOI".refined[NonEmpty], Site.GS, false)
  case Igrins2    extends Instrument("Igrins2", "IGRINS2", "IGRINS2", "IGRINS2".refined[NonEmpty], Site.GN, false)
  case Nifs       extends Instrument("Nifs", "NIFS", "NIFS", "NIFS".refined[NonEmpty], Site.GN, false)
  case Niri       extends Instrument("Niri", "NIRI", "NIRI", "NIRI".refined[NonEmpty], Site.GN, false)
  case VisitorGS  extends Instrument("VisitorGS", "Visitor Instrument", "Visitor Instrument", "VISITORGN".refined[NonEmpty], Site.GS, false)
  case VisitorGN  extends Instrument("VisitorGN", "Visitor Instrument", "Visitor Instrument", "VISITORGS".refined[NonEmpty], Site.GS, false)
  case Scorpio    extends Instrument("Scorpio", "SCORPIO", "Scorpio", "SCORPIO".refined[NonEmpty], Site.GS, false)
  case Alopeke    extends Instrument("Alopeke", "ALOPEKE", "Alopeke", "ALOPEKE".refined[NonEmpty], Site.GN, false)
  case Zorro      extends Instrument("Zorro", "ZORRO", "Zorro", "ZORRO".refined[NonEmpty], Site.GS, false)
