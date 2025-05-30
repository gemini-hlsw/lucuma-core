// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import lucuma.core.util.time.*
import munit.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*

import java.time.*
import java.time.temporal.*
import java.time.temporal.ChronoUnit

class TimeSuite extends DisciplineSuite:
  private val nanosList = List((0, false), (499999999, false), (500000000, true), (500000001, true), (999999999, true))
  private val secondsMinutesList = List((0, false), (29, false), (30, true), (31, true), (59, true))
  private val hoursList = List((0, false), (11, false), (12, true), (23, true))

  case class TestCase(setField: TemporalField, roundUnit: TemporalUnit, valuesDeltas: List[(Int, Boolean)])

  class TimeChecker[T <: Temporal](
    setField: (T, TemporalField, Long) => T,
    round: (T, TemporalUnit) => T,
    truncate: (T, TemporalUnit) => T,
    plus: (T, Long, TemporalUnit) => T,
    testCases: List[TestCase]
  ):
    private def checkField(t: T, fieldToSet: TemporalField, roundUnit: TemporalUnit, valueDeltas: List[(Int, Boolean)]) =
      valueDeltas.foreach: (value, shouldCeil) =>
        val actual = setField(t, fieldToSet, value)
        val rounded = round(actual, roundUnit)
        val expected = plus(truncate(t, roundUnit), if(shouldCeil) 1 else 0, roundUnit)
        assertEquals(rounded, expected)

    def check(t: T) =
      testCases.foreach: (tc) =>
        checkField(t, tc.setField, tc.roundUnit, tc.valuesDeltas)


  test("ZonedDateTime.roundTo"):
    val checker = TimeChecker[ZonedDateTime](_.`with`(_, _), _.roundTo(_), _.truncatedTo(_), _.plus(_, _), List(
      TestCase(ChronoField.SECOND_OF_MINUTE, ChronoUnit.MINUTES, secondsMinutesList),
      TestCase(ChronoField.MINUTE_OF_HOUR, ChronoUnit.HOURS, secondsMinutesList),
      TestCase(ChronoField.HOUR_OF_DAY, ChronoUnit.DAYS, hoursList),
    ))
    forAll: (zdt: ZonedDateTime) =>
      checker.check(zdt)

  test("Instant.roundTo"):
    val checker = TimeChecker[Instant](_.`with`(_, _), _.roundTo(_), _.truncatedTo(_), _.plus(_, _), List(
      TestCase(ChronoField.NANO_OF_SECOND, ChronoUnit.SECONDS, nanosList)
    ))
    forAll: (i: Instant) =>
      checker.check(i)

  test("LocalDateTime.roundTo"):
    val checker = TimeChecker[LocalDateTime](_.`with`(_, _), _.roundTo(_), _.truncatedTo(_), _.plus(_, _), List(
      TestCase(ChronoField.SECOND_OF_MINUTE, ChronoUnit.MINUTES, secondsMinutesList),
      TestCase(ChronoField.MINUTE_OF_HOUR, ChronoUnit.HOURS, secondsMinutesList),
      TestCase(ChronoField.HOUR_OF_DAY, ChronoUnit.DAYS, hoursList),
    ))
    forAll: (ldt: LocalDateTime) =>
      checker.check(ldt)

  test("LocalTime.roundTo"):
    val checker = TimeChecker[LocalTime](_.`with`(_, _), _.roundTo(_), _.truncatedTo(_), _.plus(_, _), List(
      TestCase(ChronoField.SECOND_OF_MINUTE, ChronoUnit.MINUTES, secondsMinutesList),
      TestCase(ChronoField.MINUTE_OF_HOUR, ChronoUnit.HOURS, secondsMinutesList)
    ))
    forAll: (lt: LocalTime) =>
      checker.check(lt)

  test("OffsetDateTime.roundTo"):
    val checker = TimeChecker[OffsetDateTime](_.`with`(_, _), _.roundTo(_), _.truncatedTo(_), _.plus(_, _), List(
      TestCase(ChronoField.SECOND_OF_MINUTE, ChronoUnit.MINUTES, secondsMinutesList),
      TestCase(ChronoField.MINUTE_OF_HOUR, ChronoUnit.HOURS, secondsMinutesList),
      TestCase(ChronoField.HOUR_OF_DAY, ChronoUnit.DAYS, hoursList),
    ))
    forAll: (odt: OffsetDateTime) =>
      checker.check(odt)

  test("OffsetTime.roundTo"):
    val checker = TimeChecker[OffsetTime](_.`with`(_, _), _.roundTo(_), _.truncatedTo(_), _.plus(_, _), List(
      TestCase(ChronoField.SECOND_OF_MINUTE, ChronoUnit.MINUTES, secondsMinutesList),
      TestCase(ChronoField.MINUTE_OF_HOUR, ChronoUnit.HOURS, secondsMinutesList)
    ))
    forAll: (ot: OffsetTime) =>
      checker.check(ot)
