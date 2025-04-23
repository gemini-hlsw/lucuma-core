// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.collections.Diet
import cats.implicits.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.data.Metadata
import lucuma.core.data.PerSite
import lucuma.core.util.Enumerated
import lucuma.refined.*
import org.typelevel.cats.time.instantInstances

import java.time.Instant

/**
 * Enumerated type for instruments.
 *
 * @group Enumerations
 */
enum Instrument(val tag: String, val shortName: String, val longName: String, val referenceName: NonEmptyString, val obsolete: Boolean) derives Enumerated:
  /** @group Constructors */ case AcqCam     extends Instrument("AcqCam", "AcqCam", "Acquisition Camera", "ACQCAM".refined[NonEmpty], false)
  /** @group Constructors */ case Flamingos2 extends Instrument("Flamingos2", "Flamingos2", "Flamingos 2", "F2".refined[NonEmpty], false)
  /** @group Constructors */ case Ghost      extends Instrument("Ghost", "GHOST", "GHOST", "GHOST".refined[NonEmpty], false)
  /** @group Constructors */ case GmosNorth  extends Instrument("GmosNorth", "GMOS-N", "GMOS North", "GMOSN".refined[NonEmpty], false)
  /** @group Constructors */ case GmosSouth  extends Instrument("GmosSouth", "GMOS-S", "GMOS South", "GMOSS".refined[NonEmpty], false)
  /** @group Constructors */ case Gnirs      extends Instrument("Gnirs", "GNIRS", "GNIRS", "GNIRS".refined[NonEmpty], false)
  /** @group Constructors */ case Gpi        extends Instrument("Gpi", "GPI", "GPI", "GPI".refined[NonEmpty], false)
  /** @group Constructors */ case Gsaoi      extends Instrument("Gsaoi", "GSAOI", "GSAOI", "GSAOI".refined[NonEmpty], false)
  /** @group Constructors */ case Igrins2    extends Instrument("Igrins2", "IGRINS2", "IGRINS2", "IGRINS2".refined[NonEmpty], false)
  /** @group Constructors */ case Niri       extends Instrument("Niri", "NIRI", "NIRI", "NIRI".refined[NonEmpty], false)
  /** @group Constructors */ case Visitor    extends Instrument("Visitor", "Visitor Instrument", "Visitor Instrument", "VISITOR".refined[NonEmpty], false)
  /** @group Constructors */ case Scorpio    extends Instrument("Scorpio", "SCORPIO", "Scorpio", "SCORPIO".refined[NonEmpty], false)
  /** @group Constructors */ case Alopeke    extends Instrument("Alopeke", "ALOPEKE", "Alopeke", "ALOPEKE".refined[NonEmpty], false)
  /** @group Constructors */ case Zorro      extends Instrument("Zorro", "ZORRO", "Zorro", "ZORRO".refined[NonEmpty], false)

  def availability(using md: Metadata): PerSite[Diet[Instant]] =
    md.instrumentAvailability.getOrElse(this, PerSite.unfold(_ => Diet.empty))

  def availabilityAt(when: Instant)(using Metadata): Set[Site] =
    availability.filter(_.contains(when))

  def availabilityAt(site: Site)(using Metadata): Diet[Instant] =
    availability.forSite(site)

  def isAvailableAt(site: Site, when: Instant)(using Metadata): Boolean =
    availabilityAt(site).contains(when)
