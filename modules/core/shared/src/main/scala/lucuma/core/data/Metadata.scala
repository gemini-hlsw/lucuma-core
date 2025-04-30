// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data

import lucuma.core.enums.Instrument
import lucuma.core.enums.Site

/** 
 * A bundle of lookup tables and similar slow-changing but dynamic information
 * from the ODB, intended to be loaded at application startup and passed 
 * around implicitly. Rather than using this instance directly, methods on 
 * data classes can provide better-placed methods that demand `Metadata`.
 */
trait Metadata:
  def availability(i: Instrument): Availability

object Metadata:

  /** A placeholder metadata until the server-side fetch is implemented. */
  val placeholder: Metadata =
    new Metadata:      
      def availability(i: Instrument): Availability =
        i match
          // GN
          case Instrument.GmosNorth => Availability.always(Site.GN)
          case Instrument.Gnirs     => Availability.always(Site.GN)
          case Instrument.Gpi       => Availability.always(Site.GN)
          case Instrument.Igrins2   => Availability.always(Site.GN)
          case Instrument.Niri      => Availability.always(Site.GN)
          case Instrument.Alopeke   => Availability.always(Site.GN)        
          // GS
          case Instrument.GmosSouth => Availability.always(Site.GS)
          case Instrument.Flamingos2=> Availability.always(Site.GS)
          case Instrument.Ghost     => Availability.always(Site.GS)
          case Instrument.Gsaoi     => Availability.always(Site.GS)
          case Instrument.Zorro     => Availability.always(Site.GS)        
          // Todo: Need north and south versions of these
          case Instrument.AcqCam    => Availability.Never
          case Instrument.Visitor   => Availability.Never          
          // Nowhere yet
          case Instrument.Scorpio   => Availability.Never
