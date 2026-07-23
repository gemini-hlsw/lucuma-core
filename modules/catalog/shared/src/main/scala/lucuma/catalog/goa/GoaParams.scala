// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import lucuma.catalog.goa.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import org.http4s.Uri

import java.time.LocalDate
import java.time.format.DateTimeFormatter

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

  private val dateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd")

  /**
   * The GOA query URL for these params against `baseUri`
   */
  def toUri(params: GoaParams, baseUri: Uri = GoaClient.DefaultBaseUri): Option[Uri] =
    params.instrument.goaName.map: goaInstr =>
      val base = baseUri / "jsonsummary" / "notengineering" / "NotFail" / goaInstr / "OBJECT"

      val withCoords = params match
        case Sidereal(coords, _, searchRadius, _)        =>
          val raDeg    = coords.ra.toAngle.toDoubleDegrees
          val decDeg   = coords.dec.toAngle.toSignedDoubleDegrees
          val srArcsec = searchRadius.toSignedDoubleDecimalArcseconds
          base / s"ra=$raDeg" / s"dec=$decDeg" / s"sr=$srArcsec"
        case NonSidereal(targetName, _, searchRadius, _) =>
          val srArcsec = searchRadius.toSignedDoubleDecimalArcseconds
          base / s"object=$targetName" / s"sr=$srArcsec"

      params.dateRange.fold(withCoords): (start, end) =>
        val startStr = start.format(dateFormatter)
        val endStr   = end.format(dateFormatter)
        withCoords / s"daterange=$startStr-$endStr"
