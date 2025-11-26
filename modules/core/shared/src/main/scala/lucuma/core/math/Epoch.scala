// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.Interval as RefinedInterval
import lucuma.core.math.JulianDate.SecondsPerDay
import lucuma.core.math.parser.EpochParsers
import lucuma.core.optics.Format
import lucuma.core.optics.syntax.all.*
import monocle.Prism
import org.typelevel.cats.time.given

import java.time.*

/**
  * An epoch, the astronomer's equivalent of `Instant`, based on a fractional year in some temporal
  * scheme (only Julian supported) that determines an equinox instant and the length of a year. The only
  * meaningful operation for an `Epoch` is to ask the elapsed epoch-years between it and some other
  * point in time. We need this for proper motion corrections because velocities are measured in
  * motion per epoch-year. The epoch year is stored internally as integral milliyears, which are 
  * computed based on Terrestial Time.
  * @param scheme This `Epoch`'s temporal scheme.
  * @see The Wikipedia [[https://en.wikipedia.org/wiki/Epoch_(astronomy) article]]
  */
final class Epoch private (val scheme: Epoch.Scheme, val toMilliyears: Epoch.IntMilliYear) {

  /** This `Epoch`'s year. Note that this value is not very useful without the `Scheme`. */
  def epochYear: Double =
    toMilliyears.value.toDouble / 1000.0

  /** Offset in epoch-years from this `Epoch` to the given `Instant`. */
  def untilInstant(i: Instant): Option[Double] = 
    scheme.fromInstant(i).map(e => untilEpochYear(e.epochYear))

  def unsafeUntilInstant(i: Instant): Double = 
    untilInstant(i).get

  /** Offset in epoch-years from this `Epoch` to the given `LocalDateTime`. */
  def untilUtcDateTime(ldt: LocalDateTime): Option[Double] = 
    scheme.fromUtcDateTime(ldt).map(e => untilEpochYear(e.epochYear))

  def unsafeUntilUtcDateTime(ldt: LocalDateTime): Double = 
    untilUtcDateTime(ldt).get

  /** Offset in epoch-years from this `Epoch` to the given epoch year under the same scheme. */
  def untilEpochYear(epochYear: Double): Double = 
    epochYear - this.epochYear

  def plusEpochYears(y: Double): Option[Epoch] = 
    scheme.fromEpochYears(epochYear + y)

  /** Convert this `Epoch` to a Java `Instant`. */
  def toInstant: Instant =
    scheme.toInstant(toMilliyears)

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
  type Year         = RefinedInterval.Closed[1972, 3000]
  type MilliYear    = RefinedInterval.Closed[1972000, 3000999]
  type IntYear      = Int Refined Year
  type IntMilliYear = Int Refined MilliYear

  object IntMilliYear extends RefinedTypeOps.Numeric[IntMilliYear, Int]

  /**
    * Standard epoch.
    * @group Constructors
    */
  lazy val J2000: Epoch = Julian.unsafeFromTerrestrialInstant(JulianEquinoxInstant)

  lazy val MinValue: Epoch = Epoch(Julian, IntMilliYear.MinValue)
  lazy val MaxValue: Epoch = Epoch(Julian, IntMilliYear.MaxValue)

  /**
    * The scheme defines an equinox instant and length of a year in terms of terrestrial days.
    * We only support Julian scheme.
    */
  sealed abstract class Scheme(
    val prefix:         Char,
    val equinoxInstant: TerrestrialInstant,
    val equinoxValue:   Double,
    val daysInYear:     Double
  ) {
    def fromTerrestrialInstant(ttInstant: TerrestrialInstant): Option[Epoch] =
      val yearsDelta: Double = (Duration.between(equinoxInstant.value, ttInstant.value).toSeconds / SecondsPerDay.toDouble) / daysInYear
      val milliYears: Double = (equinoxValue + yearsDelta) * 1000.0
      val milliYearsRounded: Int = math.round(milliYears).toInt
      val milliYearsRefined: Option[IntMilliYear] = refineV[Epoch.MilliYear](milliYearsRounded).toOption
      milliYearsRefined.map(mys => Epoch(this, mys))

    def unsafeFromTerrestrialInstant(ttInstant: TerrestrialInstant): Epoch =
      fromTerrestrialInstant(ttInstant).get

    def fromInstant(instant: Instant): Option[Epoch] =
      TerrestrialInstant.fromInstant(instant).flatMap(fromTerrestrialInstant(_))

    def unsafeFromInstant(instant: Instant): Epoch =
      fromInstant(instant).get

    def fromIntYears(years: Epoch.IntYear): Option[Epoch] = 
      fromIntMilliyears(years.value * 1000)

    def unsafeFromIntYears(years: Epoch.IntYear): Epoch = 
      fromIntYears(years).get

    def fromMilliyears(mys: IntMilliYear): Epoch = 
      new Epoch(this, mys)

    def fromIntMilliyears(mys: Int): Option[Epoch] = 
      refineV[Epoch.MilliYear](mys).map(new Epoch(this, _)).toOption

    def unsafeFromMilliyears(mys: Int): Epoch = 
      fromIntMilliyears(mys).get

    def fromUtcDateTime(ldt: LocalDateTime): Option[Epoch] = 
      fromInstant(ldt.toInstant(ZoneOffset.UTC))

    def unsafeFromUtcDateTime(ldt: LocalDateTime): Epoch =
      fromUtcDateTime(ldt).get

    def fromEpochYears(epochYear: Double): Option[Epoch] = 
      fromIntMilliyears((epochYear * 1000.0).toInt)

    def unsafeFromEpochYears(epochYear: Double): Epoch = 
      fromEpochYears(epochYear).get

    def toTerrestrialInstant(milliYears: Epoch.IntMilliYear): TerrestrialInstant = 
      val yearsSinceEquinoxInstant: Double = milliYears.value.toDouble / 1000.0 - equinoxValue
      TerrestrialInstant:
        equinoxInstant.value.plusSeconds((yearsSinceEquinoxInstant * daysInYear * SecondsPerDay.toDouble).toLong)

    /** Convert epoch year to Java `Instant`.
      *
      * Converts the epoch year to a TerrestrialInstant using the scheme's equinox instant
      * and year length, then to Java Instant.
      */
    def toInstant(milliYears: Epoch.IntMilliYear): Instant = 
      toTerrestrialInstant(milliYears).toInstant.get // There's always an Instant between 1972 and 3000

  }

  object Scheme {
    given Order[Scheme] =
      Order.by(s => (s.prefix, s.equinoxInstant, s.equinoxValue, s.daysInYear))

    given Show[Scheme] =
      Show.fromToString
  }

  /**
    * Module of constructors for Julian epochs.
    * @group Constructors
    */
  // January 1.5, 2000 TT (2000-Jan-01 11:58:55.816 UTC)
  lazy val JulianEquinoxInstant: TerrestrialInstant =
    TerrestrialInstant.unsafeFromInstant:
      Instant.from(ZonedDateTime.of(LocalDateTime.of(2000, 1, 1, 11, 58, 55, 816000000), ZoneOffset.UTC))

  case object Julian extends Scheme('J', JulianEquinoxInstant, 2000.0, 365.25)

  given Order[Epoch] =
    Order.by(e => (e.scheme, e.toMilliyears.value))

  given Show[Epoch] =
    Show.fromToString

}

trait EpochOptics { this: Epoch.type =>

  val fromString: Prism[String, Epoch] =
    Prism[String, Epoch](s => EpochParsers.epoch.parseAll(s).toOption)(e =>
      f"${e.scheme.prefix}%s${e.toMilliyears.value / 1000}%d.${e.toMilliyears.value % 1000}%03d"
    )

  val fromStringNoScheme: Format[String, Epoch] =
    Format(
      s => EpochParsers.epochLenientNoScheme.parseAll(s).toOption,
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
