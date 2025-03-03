// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.core.math.parser.AngleParsers
import lucuma.core.model.LocalObservingNight

import java.time.LocalDateTime
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("deg2hms")
def deg2hms(deg: Double): String =
  HourAngle.fromStringHMS.reverseGet(HourAngle.fromDoubleDegrees(deg))

@JSExportTopLevel("deg2dms")
def deg2dms(deg: Double): String =
  Angle.fromStringSignedDMS.reverseGet(HourAngle.fromDoubleDegrees(deg))

@JSExportTopLevel("hms2deg")
def hms2deg(hms: String): Double =
  AngleParsers.hms.parseAll(hms) match
    case Left(value)  => throw new IllegalArgumentException(value.toString)
    case Right(value) => value.toDoubleDegrees

@JSExportTopLevel("dms2deg")
def dms2deg(dms: String): Double =
  AngleParsers.dms.parseAll(dms) match
    case Left(value)  => throw new IllegalArgumentException(value.toString)
    case Right(value) => value.toDoubleDegrees

@JSExportTopLevel("dateToLocalObservingNight")
def dateToLocalObservingNight(date: js.Date): String =
  val localDate = LocalDateTime.of(date.getFullYear().toInt,
                                   date.getMonth().toInt + 1,
                                   date.getDate().toInt,
                                   date.getHours().toInt,
                                   date.getMinutes().toInt,
                                   date.getSeconds().toInt
  )
  LocalObservingNight.fromLocalDateTime(localDate).format
