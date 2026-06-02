// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates

import java.time.LocalDate

sealed trait GoaParams:
  def instrument: Instrument
  def searchRadius: Angle
  def dateRange: Option[(LocalDate, LocalDate)]

object GoaParams:

  final case class Sidereal(
    coords:       Coordinates,
    instrument:   Instrument,
    searchRadius: Angle,
    dateRange:    Option[(LocalDate, LocalDate)] = None
  ) extends GoaParams

  final case class NonSidereal(
    targetName:   String,
    instrument:   Instrument,
    searchRadius: Angle,
    dateRange:    Option[(LocalDate, LocalDate)] = None
  ) extends GoaParams
