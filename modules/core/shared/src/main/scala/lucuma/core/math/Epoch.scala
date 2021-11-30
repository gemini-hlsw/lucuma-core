// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric.{ Interval => RefinedInterval }
import eu.timepit.refined.refineV
import lucuma.core.math.parser.EpochParsers
import lucuma.core.optics.Format
import lucuma.core.optics.syntax.all._
import lucuma.core.syntax.parser._
import monocle.Prism

import java.time._

/**
  * An epoch, the astronomer's equivalent of `Instant`, based on a fractional year in some temporal
  * scheme (Julian or Besselian) that determines year zero and the length of a year. The only
  * meaningful operation for an `Epoch` is to ask the elapsed epoch-years between it and some other
  * point in time. We need this for proper motion corrections because velocities are measured in
  * motion per epoch-year. The epoch year is stored internally as integral milliyears.
  * @param scheme This `Epoch`'s temporal scheme.
  * @see The Wikipedia [[https://en.wikipedia.org/wiki/Epoch_(astronomy) article]]
  */
final class Epoch private (val scheme: Epoch.Scheme, val toMilliyears: Epoch.IntMilliYear) {

  /** This `Epoch`'s year. Note that this value is not very useful without the `Scheme`. */
  def epochYear: Double =
    toMilliyears.value.toDouble / 1000.0

  /** Offset in epoch-years from this `Epoch` to the given `Instant`. */
  def untilInstant(i: Instant): Double =
    untilLocalDateTime(LocalDateTime.ofInstant(i, ZoneOffset.UTC))

  /** Offset in epoch-years from this `Epoch` to the given `LocalDateTime`. */
  def untilLocalDateTime(ldt: LocalDateTime): Double =
    untilJulianDay(Epoch.Scheme.toJulianDay(ldt))

  /** Offset in epoch-years from this `Epoch` to the given fractional Julian day. */
  def untilJulianDay(jd: Double): Double =
    untilEpochYear(scheme.fromJulianDayToEpochYears(jd))

  /** Offset in epoch-years from this `Epoch` to the given epoch year under the same scheme. */
  def untilEpochYear(epochYear: Double): Double =
    epochYear - this.epochYear

  def plusYears(y: Double): Option[Epoch] =
    scheme.fromEpochYears(epochYear + y)

  override def equals(a: Any): Boolean =
    a match {
      case e: Epoch => (scheme === e.scheme) && toMilliyears === e.toMilliyears
      case _        => false
    }

  override def hashCode: Int =
    scheme.hashCode ^ toMilliyears.value

  override def toString: String =
    Epoch.fromString.asFormat.taggedToString("Epoch", this)

}

object Epoch extends EpochOptics {
  type Year         = RefinedInterval.Closed[1900, 3000]
  type MilliYear    = RefinedInterval.Closed[1900000, 3000999]
  type IntYear      = Int Refined Year
  type IntMilliYear = Int Refined MilliYear

  /**
    * Standard epoch.
    * @group Constructors
    */
  val J2000: Epoch = Julian.fromIntegralYears(refineMV[Year](2000))

  /**
    * Standard epoch prior to J2000. Obsolete but still in use.
    * @group Constructors
    */
  val B1950: Epoch = Besselian.fromIntegralYears(refineMV[Year](1950))

  /**
    * The scheme defines year zero and length of a year in terms of Julian days. There are two
    * common schemes that we support here.
    */
  sealed abstract class Scheme(
    val prefix:       Char,
    val yearBasis:    Double,
    val julianBasis:  Double,
    val lengthOfYear: Double
  ) {

    def fromIntegralYears(years: Epoch.IntYear): Epoch =
      fromMilliyearsUnsafe(years.value * 1000)

    def fromMilliyears(mys: IntMilliYear): Epoch =
      new Epoch(this, mys)

    def fromIntMilliyears(mys: Int): Option[Epoch] =
      refineV[Epoch.MilliYear](mys).map(new Epoch(this, _)).toOption

    def fromMilliyearsUnsafe(mys: Int): Epoch =
      fromIntMilliyears(mys).get

    def fromLocalDateTime(ldt: LocalDateTime): Option[Epoch] =
      fromJulianDay(Scheme.toJulianDay(ldt))

    def fromJulianDay(jd: Double): Option[Epoch] =
      fromEpochYears(fromJulianDayToEpochYears(jd))

    def fromEpochYears(epochYear: Double): Option[Epoch] =
      fromIntMilliyears((epochYear * 1000.0).toInt)

    def fromLocalDateTimeToEpochYears(ldt: LocalDateTime): Double =
      fromJulianDayToEpochYears(Scheme.toJulianDay(ldt))

    def fromJulianDayToEpochYears(jd: Double): Double =
      yearBasis + (jd - julianBasis) / lengthOfYear
  }
  object Scheme {

    /**
      * Convert a `LocalDateTime` to a fractional Julian day.
      * @see The Wikipedia [[https://en.wikipedia.org/wiki/Julian_day article]]
      */
    def toJulianDay(dt: LocalDateTime): Double =
      JulianDate.ofLocalDateTime(dt).dayNumber.toDouble

    implicit val SchemeOrder: Order[Scheme] =
      Order.by(s => (s.prefix, s.yearBasis, s.julianBasis, s.lengthOfYear))

    implicit val SchemeShow: Show[Scheme] =
      Show.fromToString

  }

  /**
    * Module of constructors for Besselian epochs.
    * @group Constructors
    */
  case object Besselian extends Scheme('B', 1900.0, 2415020.31352, 365.242198781)

  /**
    * Module of constructors for Julian epochs.
    * @group Constructors
    */
  case object Julian extends Scheme('J', 2000.0, 2451545.0, 365.25)

  implicit val EpochOrder: Order[Epoch] =
    Order.by(e => (e.scheme, e.toMilliyears.value))

  implicit val EpochShow: Show[Epoch] =
    Show.fromToString

}

trait EpochOptics { this: Epoch.type =>

  val fromString: Prism[String, Epoch] =
    Prism[String, Epoch](s => EpochParsers.epoch.parseExact(s))(e =>
      f"${e.scheme.prefix}%s${e.toMilliyears.value / 1000}%d.${e.toMilliyears.value % 1000}%03d"
    )

  val fromStringNoScheme: Format[String, Epoch] =
    Format(
      s => EpochParsers.epochLenientNoScheme.parseExact(s),
      {
        case e if e.toMilliyears.value % 1000 === 0 =>
          f"${e.toMilliyears.value / 1000}%d"
        case e if e.toMilliyears.value % 100 === 0  =>
          f"${e.toMilliyears.value / 1000}%d.${(e.toMilliyears.value % 1000) / 100}%01d"
        case e if e.toMilliyears.value % 10 === 0   =>
          f"${e.toMilliyears.value / 1000}%d.${(e.toMilliyears.value % 1000) / 10}%02d"
        case e =>
          f"${e.toMilliyears.value / 1000}%d.${e.toMilliyears.value % 1000}%03d"
      }
    )

}
