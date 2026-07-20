// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

/**
 * GOA's `observation_class`, the legacy OCS `ObsClass` tag
 */
enum GoaObservationClass(val tag: String):
  case Science              extends GoaObservationClass("science")
  case DayCal               extends GoaObservationClass("dayCal")
  case PartnerCal           extends GoaObservationClass("partnerCal")
  case ProgCal              extends GoaObservationClass("progCal")
  case Acquisition          extends GoaObservationClass("acq")
  case AcquisitionCal       extends GoaObservationClass("acqCal")
  case Unknown(raw: String) extends GoaObservationClass(raw)

object GoaObservationClass:

  private val knownValues: List[GoaObservationClass] =
    List(Science, DayCal, PartnerCal, ProgCal, Acquisition, AcquisitionCal)

  def fromTag(tag: String): GoaObservationClass =
    knownValues.find(_.tag == tag).getOrElse(Unknown(tag))
