// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

/**
  * Enumerated type for GNIRS Observation Read Mode. It can be fixed for all steps of the observation,
  * or computed in each step based on the exposure time of that step. See [[GnirsReadMode.forExposureTime]]
  * for the logic to compute the read mode based on the exposure time.
  */
enum GnirsObsReadMode(val tag: String, val shortName: String, val longName: String):
  case AutomaticInEachStep extends GnirsObsReadMode("Automatic", "Automatic", "Automatic in each step")
  case Fixed(readMode: GnirsReadMode) extends GnirsObsReadMode(readMode.tag, readMode.shortName, readMode.longName)

  def resolveForStepExposureTime(t: TimeSpan): GnirsReadMode =
    this match
      case AutomaticInEachStep => GnirsReadMode.forExposureTime(t)
      case Fixed(readMode) => readMode

object GnirsObsReadMode:
  given Enumerated[GnirsObsReadMode] with
    def all: List[GnirsObsReadMode] =
      AutomaticInEachStep +: Enumerated[GnirsReadMode].all.map(Fixed(_))

    def tag(a: GnirsObsReadMode): String = a.tag

  given Display[GnirsObsReadMode] = Display.by(_.shortName, _.longName)
