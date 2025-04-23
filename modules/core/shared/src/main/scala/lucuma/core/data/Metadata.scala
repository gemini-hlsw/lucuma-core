// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data

import cats.collections.Diet
import cats.collections.Range
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.util.Enumerated

import java.time.Instant 

/** 
 * A bundle of lookup tables and similar slow-changing but dynamic information
 * from the ODB, intended to be loaded at application startup and passed 
 * around implicitly. Rather than using this instance directly, methods on 
 * data classes can provide better-placed methods that demand `Metadata`.
 */
trait Metadata:
  def instrumentAvailability: Map[Instrument, PerSite[Diet[Instant]]]

object Metadata:

  /** 
   * A placeholder metadata until the server-side fetch is implemented. 
   * 
   */
  val placeholder: Metadata =
    new Metadata:      

      val instrumentAvailability: Map[Instrument, PerSite[Diet[Instant]]] =
        val always = Diet.fromRange(Range(Instant.MIN, Instant.MAX))
        val never  = Diet.empty[Instant]
        Enumerated[Instrument]
          .all.fproduct:

            // gn
            case Instrument.GmosNorth  |
                 Instrument.Gnirs      |
                 Instrument.Gpi        |
                 Instrument.Igrins2    |
                 Instrument.Niri       |
                 Instrument.Alopeke    => PerSite(always, never)

            // gs
            case Instrument.GmosSouth  |
                 Instrument.Flamingos2 |
                 Instrument.Ghost      |
                 Instrument.Gsaoi      |
                 Instrument.Zorro      => PerSite(never, always)

            // both
            case Instrument.AcqCam     |
                 Instrument.Visitor    => PerSite(always, always)

            // future
            case Instrument.Scorpio    => PerSite(never, never)

          .toMap


