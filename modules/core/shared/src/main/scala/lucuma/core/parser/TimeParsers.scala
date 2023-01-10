// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.parser

import cats.parse.Parser.oneOf
import cats.parse.Parser.string
import cats.parse.Rfc5234.sp
import cats.parse.*
import cats.syntax.all.*

import java.time.DateTimeException
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.Month
import java.time.Year
import java.time.ZoneOffset

/** Parsers for `java.time` data types. */
trait TimeParsers {

  import MiscParsers.intN

  /** Catch a `DateTimeException`, useful for flatMap. */
  def catchDTE[A, B](f: A => B): A => Option[B] =
    a =>
      try Some(f(a))
      catch { case e: DateTimeException => None }

  /** Parser for 4 consecutive digits, parsed as a `Year`. */
  val year4: Parser[Year] =
    intN(4).mapFilter(catchDTE(Year.of)).withContext("year4")

  /** Parser for 2 consecutive digits, parsed as a `Month`. */
  val month2: Parser[Month] =
    intN(2).mapFilter(catchDTE(Month.of)).withContext("month2")

  /** Parser for 3 letter month strings like "Jan", parsed as a `Month`. */
  val monthMMM: Parser[Month] = {
    import Month.*

    val months = List(
      "Jan" -> JANUARY,
      "Feb" -> FEBRUARY,
      "Mar" -> MARCH,
      "Apr" -> APRIL,
      "May" -> MAY,
      "Jun" -> JUNE,
      "Jul" -> JULY,
      "Aug" -> AUGUST,
      "Sep" -> SEPTEMBER,
      "Oct" -> OCTOBER,
      "Nov" -> NOVEMBER,
      "Dec" -> DECEMBER
    )

    oneOf(months.map { case (s, m) => string(s).as(m) }).withContext("monthMMM")
  }

  /**
   * Parser for instants in UTC, where the date is followed by the time and
   * separated by spaces.
   */
  def instantUTC(date: Parser[LocalDate], time: Parser[LocalTime]): Parser[Instant] =
    (date <* sp, time).mapN {
      case (d, t) =>
        LocalDateTime.of(d, t).toInstant(ZoneOffset.UTC)
    }
}
object TimeParsers extends TimeParsers
