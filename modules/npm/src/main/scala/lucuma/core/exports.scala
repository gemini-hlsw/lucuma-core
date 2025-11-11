// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.core.math.parser.AngleParsers
import lucuma.core.model.*
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.WithGid
import spire.math.Rational

import java.time.LocalDateTime
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

private def roundedAngle(deg: Double, factor: Int): HourAngle =
  val microseconds =
    Rational(HourAngle.fromDoubleDegrees(deg).toMicroseconds, factor).round.toLong * factor
  HourAngle.fromMicroseconds(microseconds)

@JSExportTopLevel("deg2hms")
def deg2hms(deg: Double): String =
  HourAngle.fromStringHMS.reverseGet(roundedAngle(deg, 1000)).dropRight(3)

@JSExportTopLevel("deg2dms")
def deg2dms(deg: Double): String =
  Angle.fromStringSignedDMS
    .reverseGet(roundedAngle(deg, 10000))
    .dropRight(4)
    .replace("-00:00:00.00", "+00:00:00.00")

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

@JSExportTopLevel("signedArcSeconds")
def signedArcSeconds(arcseconds: Double | String) =
  val angle = arcseconds match
    case s: String => Angle.fromBigDecimalArcseconds(BigDecimal(s))
    case d: Double => Angle.fromDoubleArcseconds(d)
  Angle.signedDecimalArcseconds.get(angle).doubleValue

@JSExportTopLevel("dateToLocalObservingNight")
def dateToLocalObservingNight(date: js.Date): String =
  val localDate = LocalDateTime.of(
    date.getFullYear().toInt,
    date.getMonth().toInt + 1,
    date.getDate().toInt,
    date.getHours().toInt,
    date.getMinutes().toInt,
    date.getSeconds().toInt
  )
  LocalObservingNight.fromLocalDateTime(localDate).toLocalDate.toString()

private def tryParseId(maybeId: String, withGid: WithGid): js.UndefOr[String] =
  withGid.Id.parse(maybeId).orUndefined.map(_.toString)

@JSExportTopLevel("parseAttachmentId")
def parseAttachmentId(maybeAttachmentId: String): js.UndefOr[String] =
  tryParseId(maybeAttachmentId, Attachment)

@JSExportTopLevel("parseCallForProposalsId")
def parseCallForProposalsId(maybeCallForProposalsId: String): js.UndefOr[String] =
  tryParseId(maybeCallForProposalsId, CallForProposals)

@JSExportTopLevel("parseConfigurationRequestId")
def parseConfigurationRequestId(maybeConfigurationRequestId: String): js.UndefOr[String] =
  tryParseId(maybeConfigurationRequestId, ConfigurationRequest)

@JSExportTopLevel("parseDatasetId")
def parseDatasetId(maybeDatasetId: String): js.UndefOr[String] =
  tryParseId(maybeDatasetId, Dataset)

@JSExportTopLevel("parseExecutionEventId")
def parseExecutionEventId(maybeExecutionEventId: String): js.UndefOr[String] =
  tryParseId(maybeExecutionEventId, ExecutionEvent)

@JSExportTopLevel("parseGroupId")
def parseGroupId(maybeGroupId: String): js.UndefOr[String] =
  tryParseId(maybeGroupId, Group)

@JSExportTopLevel("parseObservationId")
def parseObservationId(maybeObservationId: String): js.UndefOr[String] =
  tryParseId(maybeObservationId, Observation)

@JSExportTopLevel("parseProgramId")
def parseProgramId(maybeProgramId: String): js.UndefOr[String] =
  tryParseId(maybeProgramId, Program)

@JSExportTopLevel("parseProgramNoteId")
def parseProgramNoteId(maybeProgramNoteId: String): js.UndefOr[String] =
  tryParseId(maybeProgramNoteId, ProgramNote)

@JSExportTopLevel("parseProgramUserId")
def parseProgramUserId(maybeProgramUserId: String): js.UndefOr[String] =
  tryParseId(maybeProgramUserId, ProgramUser)

@JSExportTopLevel("parseStandardRoleId")
def parseStandardRoleId(maybeStandardRoleId: String): js.UndefOr[String] =
  tryParseId(maybeStandardRoleId, StandardRole)

@JSExportTopLevel("parseTargetId")
def parseTargetId(maybeTargetId: String): js.UndefOr[String] =
  tryParseId(maybeTargetId, Target)

@JSExportTopLevel("parseUserId")
def parseUserId(maybeUserId: String): js.UndefOr[String] =
  tryParseId(maybeUserId, User)

@JSExportTopLevel("parseVisitId")
def parseVisitId(maybeVisitId: String): js.UndefOr[String] =
  tryParseId(maybeVisitId, Visit)
