// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Monoid
import cats.Order
import cats.syntax.order.*
import eu.timepit.refined.types.numeric.NonNegLong
import lucuma.core.optics.Format
import lucuma.core.refined.numeric.NonZeroBigDecimal
import lucuma.core.refined.numeric.NonZeroInt
import monocle.Iso
import monocle.Prism

import java.time.Duration
import java.time.temporal.ChronoUnit.MICROS
import java.time.temporal.TemporalUnit
import scala.annotation.targetName
import scala.util.Try

/**
 * TimeSpan is a time span, similar to a `FiniteDuration` in that it is a
 * single `Long` value representing an amount of time, but in this case fixed
 * to positive microseconds. This corresponds with the ODB GraphQL schema
 * duration, which allows setting and displaying times in Long microseconds
 * and fractional milliseconds, seconds, minutes, and hours.
 */
opaque type TimeSpan = NonNegLong

object TimeSpan {

  val Min: TimeSpan =
    NonNegLong.MinValue

  val Max: TimeSpan =
    NonNegLong.MaxValue

  inline def Zero: TimeSpan = Min

  /**
   * Constructs a TimeSpan from the given number of microseconds, if it is
   * non-negative.
   */
  def fromMicroseconds(µs: Long): Option[TimeSpan] =
    NonNegLong.from(µs).toOption

  def unsafeFromMicroseconds(µs: Long): TimeSpan =
    fromMicroseconds(µs).getOrElse(sys.error(s"The µs value ($µs) must be non-negative."))

  /**
   * Constructs a TimeSpan from the given amount of microseconds, rounding
   * the nearest microsecond and capping the lower value to Min and the
   * upper value to Max.
   */
  def fromMicrosecondsBounded(µs: BigDecimal): TimeSpan =
    µs.setScale(0, BigDecimal.RoundingMode.HALF_UP) match {
      case µsʹ if µsʹ < Min.toMicroseconds => Min
      case µsʹ if µsʹ > Max.toMicroseconds => Max
      case µsʹ                             => unsafeFromMicroseconds(µsʹ.longValue)
    }

  /**
   * Constructs a TimeSpan from a `NonNegLong` value in microseconds.
   */
  def apply(µs: NonNegLong): TimeSpan =
    fromNonNegMicroseconds(µs)

  def fromNonNegMicroseconds(µs: NonNegLong): TimeSpan =
    µs

  /**
   * Converts the Duration into a TimeSpan if it is in range, discarding any
   * sub-microsecond value.
   */
  def fromDuration(value: Duration): Option[TimeSpan] = {
    val µs = BigInt(value.getSeconds) * BigInt(10).pow(6) + value.getNano/1000L

    // would like to call µs.bigInteger.longValueExact but is not
    // available in Scala JS at the moment

    Try(BigDecimal(µs).bigDecimal.longValueExact)
      .toOption
      .flatMap(fromMicroseconds)
  }

  /**
   * Builds a Duration from the specified amount and unit and converts it
   * into a TimeSpan if it is in range, discarding any sub-microsecond value.
   */
  def fromDuration(amount: Long, unit: TemporalUnit): Option[TimeSpan] =
    fromDuration(Duration.of(amount, unit))

  def unsafeFromDuration(value: Duration): TimeSpan =
    fromDuration(value).getOrElse(sys.error(s"The duration value ($value) must be non-negative."))

  def unsafeFromDuration(amount: Long, unit: TemporalUnit): TimeSpan =
    unsafeFromDuration(Duration.of(amount, unit))

  /**
   * Converts the given amount of time in milliseconds into a TimeSpan,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   */
  def fromMilliseconds(ms: BigDecimal): Option[TimeSpan] =
    Try(ms.bigDecimal.movePointRight(3).setScale(0, java.math.RoundingMode.HALF_UP).longValueExact)
      .toOption
      .flatMap(fromMicroseconds)

  /**
   * Converts the given amount of time in seconds into a TimeSpan,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   */
  def fromSeconds(s: BigDecimal): Option[TimeSpan] =
    fromMilliseconds(s.bigDecimal.movePointRight(3))

  /**
   * Converts the given amount of time in minutes into a TimeSpan,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   */
  def fromMinutes(m: BigDecimal): Option[TimeSpan] =
    fromSeconds(m * 60L)

  /**
   * Converts the given amount of time in hours into a TimeSpan,
   * rounding any sub-microsecond value to the nearest microsecond (half-up).
   */
  def fromHours(h: BigDecimal): Option[TimeSpan] =
    fromSeconds(h * 3_600L)

  /**
   * Parses the string into a TimeSpan according to ISO-8601 standard, if possible.
   */
  def parse(iso: String): Either[String, TimeSpan] =
      Try(Duration.parse(iso))
        .toOption
        .flatMap(fromDuration)
        .toRight(s"Cannot parse `$iso` as a TimeSpan")

  extension (timeSpan: TimeSpan) {

    def isZero: Boolean =
      toMicroseconds === 0

    def nonZero: Boolean =
      !isZero

    def toMicroseconds: Long =
      timeSpan.value

    def toNonNegMicroseconds: NonNegLong =
      timeSpan

    def toMilliseconds: BigDecimal =
      BigDecimal(toMicroseconds).bigDecimal.movePointLeft(3)

    def toSeconds: BigDecimal =
      BigDecimal(toMicroseconds).bigDecimal.movePointLeft(6)

    def toMinutes: BigDecimal =
      toSeconds / 60

    def toHours: BigDecimal =
      toSeconds / 3_600

    def toDuration: Duration =
      Duration.of(timeSpan.value, MICROS)

    def toSecondsPart: Int = 
      (timeSpan.toSeconds.longValue % 60L).toInt

    def toMinutesPart: Int = 
      (timeSpan.toMinutes.longValue % 60L).toInt

    def toHoursPart: Int = 
      (timeSpan.toHours.longValue % 24L).toInt

    def toMillisPart: Int = 
      (timeSpan.toMilliseconds.longValue % 1000L).toInt

    /**
     * Formats this TimeSpan using the ISO-8601 standard.
     */
    def format: String =
      toDuration.toString

    /**
     * Adds two TimeSpan values, if the resulting value is in range.
     */
    def add(other: TimeSpan): Option[TimeSpan] =
      fromMicroseconds(timeSpan.toMicroseconds + other.toMicroseconds)

    /**
     * Adds two TimeSpan values, capping the resulting value at `Max`.
     */
    @targetName("boundedAdd")
    def +|(other: TimeSpan): TimeSpan =
      add(other).getOrElse(Max)

    /**
     * Subtracts two TimeSpan values, if the resulting value is in range.
     */
    def subtract(other: TimeSpan): Option[TimeSpan] =
      fromMicroseconds(timeSpan.toMicroseconds - other.toMicroseconds)

    /**
     * Subtracts a TimeSpan value, with a floor of `Min` on the resulting value.
     */
    @targetName("boundedSubtract")
    def -|(other: TimeSpan): TimeSpan =
      subtract(other).getOrElse(Min)

    /**
     * Multiplies a TimeSpan by an integer, limiting the resulting value to the
     * range (`Min`, `Max`).
     */
    @targetName("boundedMultiply")
    def *|(multiplier: Int): TimeSpan =
      fromMicrosecondsBounded(BigDecimal(timeSpan.toMicroseconds) * multiplier)

    @targetName("boundedMultiply")
    def *|(multiplier: BigDecimal): TimeSpan =
      fromMicrosecondsBounded(timeSpan.toMicroseconds * multiplier)

    /**
     * Divides a TimeSpan by a non-negative integer, via integer division.
     */
    @targetName("boundedDivide")
    def /|(divisor: NonZeroInt): TimeSpan =
      TimeSpan.fromMicroseconds(timeSpan.toMicroseconds / divisor.value).getOrElse(Min)

    @targetName("boundedDivide")
    def /|(divisor: NonZeroBigDecimal): TimeSpan =
      fromMicrosecondsBounded(timeSpan.toMicroseconds / divisor.value)

  }

  /**
   * Obtains the absolute (i.e., positive regardless of order) time span between two Timestamp, if in range.
   */
  def between(t0: Timestamp, t1: Timestamp): Option[TimeSpan] =
    fromDuration(
      Duration.between(t0.toInstant, t1.toInstant).abs
    )

  val NonNegMicroseconds: Iso[NonNegLong, TimeSpan] =
    Iso[NonNegLong, TimeSpan](fromNonNegMicroseconds)(toNonNegMicroseconds)

  val FromMicroseconds: Prism[Long, TimeSpan] =
    Prism(fromMicroseconds)(toMicroseconds)

  val FromMilliseconds: Format[BigDecimal, TimeSpan] =
    Format(fromMilliseconds, toMilliseconds)

  val FromSeconds: Format[BigDecimal, TimeSpan] =
    Format(fromSeconds, toSeconds)

  val FromMinutes: Format[BigDecimal, TimeSpan] =
    Format(fromMinutes, toMinutes)

  val FromHours: Format[BigDecimal, TimeSpan] =
    Format(fromHours, toHours)

  val FromDuration: Format[Duration, TimeSpan] =
    Format(fromDuration, toDuration)

  val FromString: Format[String, TimeSpan] =
    Format(parse(_).toOption, format)

  given orderTimeSpan: Order[TimeSpan] =
    Order.by(_.value)

  /**
   * TimeSpan forms a monoid under the bounded add operation.
   */
  given Monoid[TimeSpan] =
    Monoid.instance(TimeSpan.Zero, _ +| _)

}
