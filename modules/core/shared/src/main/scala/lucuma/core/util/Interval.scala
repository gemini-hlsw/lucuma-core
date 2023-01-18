// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.types.numeric.NonNegLong
import lucuma.core.optics.Format
import monocle.Iso
import monocle.Prism

import java.math.RoundingMode.HALF_UP
import java.time.Duration
import java.time.temporal.ChronoUnit.MICROS
import scala.util.Try

/**
 * Interval is a time span, similar to a `FiniteDuration` in that it is a
 * single `Long` value representing an amount of time, but in this case fixed
 * to positive microseconds. This corresponds with the ODB GraphQL schema
 * duration, which allows setting and displaying times in Long microseconds
 * and fractional milliseconds, seconds, minutes, and hours.
  */
opaque type Interval = NonNegLong

object Interval {

  val Min: Interval =
    NonNegLong.MinValue

  val Max: Interval =
    NonNegLong.MaxValue

  /**
   * Constructs an Interval from the given number of microseconds, if it is
   * non-negative.
   */
  def fromMicroseconds(µs: Long): Option[Interval] =
    NonNegLong.from(µs).toOption

  def unsafeFromMicroseconds(µs: Long): Interval =
    fromMicroseconds(µs).getOrElse(sys.error(s"The µs value ($µs) must be non-negative."))

  def fromNonNegMicroseconds(µs: NonNegLong): Interval =
    µs

  /**
   * Converts the Duration into an Interval if it is in range, discarding any
   * sub-microsecond value.
   */
  def fromDuration(value: Duration): Option[Interval] = {
    val µs = BigInt(value.getSeconds) * BigInt(10).pow(6) + value.getNano/1000L

    // would like to call µs.bigInteger.longValueExact but is not
    // available in Scala JS at the moment

    Try(BigDecimal(µs).bigDecimal.longValueExact)
      .toOption
      .flatMap(fromMicroseconds)
  }

  /**
   * Converts the given amount of time in milliseconds into an Interval,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   * If the value is too big to be represented in a Long as
   */
  def fromMilliseconds(ms: BigDecimal): Option[Interval] =
    Try(ms.bigDecimal.movePointRight(3).setScale(0, HALF_UP).longValueExact)
      .toOption
      .flatMap(fromMicroseconds)

  /**
   * Converts the given amount of time in seconds into an Interval,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   */
  def fromSeconds(s: BigDecimal): Option[Interval] =
    fromMilliseconds(s.bigDecimal.movePointRight(3))

  /**
   * Converts the given amount of time in minutes into an Interval,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   */
  def fromMinutes(m: BigDecimal): Option[Interval] =
    fromSeconds(m * 60L)

  /**
   * Converts the given amount of time in hours into an Interval,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   */
  def fromHours(h: BigDecimal): Option[Interval] =
    fromSeconds(h * 3_600L)

  /**
   * Parses the string into an Interval according to ISO-8601 standard, if possible.
   */
  def parse(iso: String): Either[String, Interval] =
      Try(Duration.parse(iso))
        .toOption
        .flatMap(fromDuration)
        .toRight(s"Cannot parse `$iso` as an Interval")

  extension (interval: Interval) {

    def toMicroseconds: Long =
      interval.value

    def toNonNegMicroseconds: NonNegLong =
      interval

    def toMilliseconds: BigDecimal =
      BigDecimal(toMicroseconds).bigDecimal.movePointLeft(3)

    def toSeconds: BigDecimal =
      BigDecimal(toMicroseconds).bigDecimal.movePointLeft(6)

    def toMinutes: BigDecimal =
      toSeconds / 60

    def toHours: BigDecimal =
      toSeconds / 3_600

    def toDuration: Duration =
      Duration.of(interval.value, MICROS)

    /**
     * Formats this Interval using the ISO-8601 standard.
     */
    def format: String =
      toDuration.toString

  }

  /**
   * Obtains the absolute (i.e., positive regardless of order) time interval between two Timestamp, if in range.
   */
  def between(t0: Timestamp, t1: Timestamp): Option[Interval] =
    fromDuration(
      Duration.between(t0.toInstant, t1.toInstant).abs
    )

  val NonNegMicroseconds: Iso[NonNegLong, Interval] =
    Iso[NonNegLong, Interval](fromNonNegMicroseconds)(toNonNegMicroseconds)

  val FromMicroseconds: Prism[Long, Interval] =
    Prism(fromMicroseconds)(toMicroseconds)

  val FromMilliseconds: Format[BigDecimal, Interval] =
    Format(fromMilliseconds, toMilliseconds)

  val FromSeconds: Format[BigDecimal, Interval] =
    Format(fromSeconds, toSeconds)

  val FromMinutes: Format[BigDecimal, Interval] =
    Format(fromMinutes, toMinutes)

  val FromHours: Format[BigDecimal, Interval] =
    Format(fromHours, toHours)

  val FromDuration: Format[Duration, Interval] =
    Format(fromDuration, toDuration)

  val FromString: Format[String, Interval] =
    Format(parse(_).toOption, format)

  given orderInterval: Order[Interval] =
    Order.by(_.value)

}
