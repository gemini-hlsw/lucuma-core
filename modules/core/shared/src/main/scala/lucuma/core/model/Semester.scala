// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import cats.Show
import cats.parse.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.numeric.Interval
import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.enums.parser.EnumParsers.half
import lucuma.core.parser.MiscParsers.intN
import monocle.Prism

import java.time.*
import java.time.Month.*

/**
 * A (year, half) tuple where the year falls in the bounds [2000, 9999].
 *
 * @group Program Model
 */
final case class Semester(yearInt: Semester.YearInt, half: Half) {

  /** The semester's year expressed as a java.time.Year. */
  def year: Year =
    Year.of(yearInt.value)

  /**
   * This Semester plus the given number of years, if the resulting year is in
   * range.
   */
  def plusYears(n: Int): Option[Semester] =
    Semester
      .YearInt
      .from(yearInt.value + n)
      .toOption
      .map(Semester(_, half))

  /**
   * This Semester plus the given number of half-years, if the resulting year
   * is in range.
   */
  def plusSemesters(n: Int): Option[Semester] = {
    val yy = yearInt.value
    val hs = yy * 2 + half.toInt + n
    Semester.YearInt.from(hs / 2).toOption.map { y =>
      Semester(y, Half.unsafeFromInt(hs % 2))
    }
  }

  /** The semester immediately following this one. */
  def next: Option[Semester] =
    plusSemesters(1)

  /** The semester immediately preceding this one. */
  def prev: Option[Semester] =
    plusSemesters(-1)

  /** Module of various representations of the first instant of the semester, *inclusive*. */
  object start {
    lazy val yearMonth: YearMonth =
      YearMonth.of(year.getValue, half.startMonth)

    lazy val localDate: LocalDate =
      LocalDate.of(year.getValue, half.startMonth, 1)

    lazy val localDateTime: LocalDateTime =
      LocalDateTime.of(localDate, LocalTime.MIDNIGHT).minusHours(10) // 2pm the day before

    def zonedDateTime(zoneId: ZoneId): ZonedDateTime =
      ZonedDateTime.of(localDateTime, zoneId)

    def atSite(site: Site): ZonedDateTime =
      zonedDateTime(site.timezone)
  }

  /**
   * Module of various representations of the last instant of the semester, *inclusive*.
   * @group Program Model
   */
  object end {
    private def nextYear: Int =
      (half match {
        case Half.A => year
        case Half.B => year.plusYears(1)
      }).getValue

    lazy val yearMonth: YearMonth =
      YearMonth.of(nextYear, half.endMonth)

    lazy val localDate: LocalDate =
      LocalDate.of(nextYear, half.endMonth, half.endMonth.maxLength)

    lazy val localDateTime: LocalDateTime =
      LocalDateTime.of(localDate.minusDays(1), LocalTime.MAX).plusHours(14) // 2pm today

    def zonedDateTime(zoneId: ZoneId): ZonedDateTime =
      ZonedDateTime.of(localDateTime, zoneId)

    def atSite(site: Site): ZonedDateTime =
      zonedDateTime(site.timezone)
  }

  /** Format as full year and semester half, `2009A`. */
  def format: String =
    s"$year$half"

  /** Format as 2-digit year and semester half, `09A`. */
  def formatShort: String =
    f"${year.getValue % 100}%02d$half"

  override def toString   =
    f"Semester($format)"

}

object Semester {

  // Limits semester to the values that are expressed in the database.
  type YearInt = Int Refined Interval.Closed[2000, 9999]
  object YearInt extends RefinedTypeOps[YearInt, Int] {
    val MinValue: YearInt = unsafeFrom(2000)
    val MaxValue: YearInt = unsafeFrom(9999)
  }

  val MinValue: Semester =
    Semester(YearInt.MinValue, Half.A)

  val MaxValue: Semester =
    Semester(YearInt.MaxValue, Half.B)

  object parse {

    val yearInt: Parser[YearInt] =
      intN(4).mapFilter(y => YearInt.from(y).toOption).withContext("yearInt")

    /** Semester parser, which must agree with the `.format` method. */
    val semester: Parser[Semester] =
      (yearInt ~ half).map(apply).withContext("semester")

  }

  /** Semester for the specified year and month. */
  def fromYearMonth(ym: YearMonth): Option[Semester] = {
    val m = ym.getMonth
    val y = m match {
      case JANUARY => ym.getYear - 1
      case _       => ym.getYear
    }
    YearInt.from(y).toOption.map { y =>
      Semester(y, Half.fromMonth(m))
    }
  }

  /**
   * Semester for the specified year and month, exclusive of semester start and inclusive of
   * semester end. The semester actually starts at 2pm on the 31st of the January/August, but
   * this method gives the entire switchover day to the previous semester. A simpler way of saying
   * this is that this method ignores the day of the month. If you need more precision you must
   * use a conversion method that includes the time of day.
   */
  def fromLocalDate(d: LocalDate): Option[Semester] =
    fromYearMonth(YearMonth.of(d.getYear, d.getMonth))

  /**
   * Semester for the specified local date and time. This handles the 2pm switchover on the last
   * day of the semester.
   */
  def fromLocalDateTime(d: LocalDateTime): Option[Semester] = {
    val dʹ = LocalObservingNight.fromLocalDateTime(d).toLocalDate
    fromYearMonth(YearMonth.of(dʹ.getYear, dʹ.getMonth))
  }

  /**
   * Semester for the specified zoned date and time. This handles the 2pm switchover on the last
   * day of the semester.
   */
  def fromZonedDateTime(d: ZonedDateTime): Option[Semester] =
    fromLocalDateTime(d.toLocalDateTime)

  /** Semester for the zoned date and time of the given Site and Instant. */
  def fromSiteAndInstant(s: Site, i: Instant): Option[Semester] =
    fromZonedDateTime(ZonedDateTime.ofInstant(i, s.timezone))

  /** Parse a full-year Semester like `2009A` from a String, if possible. */
  val fromString: Prism[String, Semester] =
    Prism[String, Semester](s => Semester.parse.semester.parseAll(s).toOption)(_.format)

  /** Parse a full-year Semester like `2009A` from a String, throwing on failure. */
  def unsafeFromString(s: String): Semester =
    fromString.getOption(s).getOrElse(sys.error(s"Invalid semester: $s"))

  /** `Semester` is ordered pairwise by its data members. */
  given Order[Semester] =
    Order.by(a => (a.yearInt.value, a.half))

  /** `Ordering` instance for Scala standard library. */
  given scala.math.Ordering[Semester] =
    Order[Semester].toOrdering

  given Show[Semester] =
    Show.fromToString

}
