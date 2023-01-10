// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Order
import cats.syntax.either.*
import cats.syntax.order.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.optics.Format
import lucuma.core.optics.SplitEpi
import monocle.Prism
import org.typelevel.cats.time.instances.instant.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset.UTC
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_DATE_TIME
import java.time.format.DateTimeFormatterBuilder
import java.time.format.DateTimeParseException
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit.MICROS
import java.util.Locale

/**
 * Timestamp is an Instant truncated and limited to fit in a database
 * `timestamp` column.  Using a `Timestamp`, we're guaranteed to safely
 * round-trip values through the database.
 */

opaque type Timestamp = Instant

object Timestamp {

  val Min: Timestamp =
    ZonedDateTime.of( -4712, 1, 1, 0, 0, 0, 0, UTC).toInstant

  val Max: Timestamp =
    ZonedDateTime.of(294275, 12, 31, 23, 59, 59, 999999000, UTC).toInstant

  /** Instant.EPOCH transformed to Timestamp */
  val Epoch: Timestamp =
    Instant.EPOCH

  /**
   * Converts valid `Instant`s to `Timestamp`.  `Instant`s with sub-microsecond
   * precision or that are above or beyond the range of `Timestamp` produce
   * `None` while valid `Instant`s produce `Some` corresponding `Timestamp`.
   */
  def fromInstant(value: Instant): Option[Timestamp] =
    Option.when(Min <= value && value <= Max && value.truncatedTo(MICROS) === value)(value)

  /**
   * Truncates any sub-microsecond precision in the given `Instant` and then
   * attempts to get the corresponding `Timestamp`.  This will be successful
   * for `Instants` that fall within the range `Min` to `Max` (inclusive).
   */
  def fromInstantTruncated(value: Instant): Option[Timestamp] =
    fromInstant(value.truncatedTo(MICROS))

  def unsafeFromInstant(value: Instant): Timestamp =
    fromInstant(value).getOrElse(sys.error(s"$value out of Timestamp range or includes sub-microsecond precision"))

  def unsafeFromInstantTruncated(value: Instant): Timestamp =
    fromInstantTruncated(value).getOrElse(sys.error(s"$value out of Timestamp range"))

  def fromLocalDateTime(value: LocalDateTime): Option[Timestamp] =
    fromInstant(value.toInstant(UTC))

  def unsafeFromLocalDateTime(value: LocalDateTime): Timestamp =
    fromLocalDateTime(value).getOrElse(sys.error(s"$value out of Timestamp range or includes sub-microsecond precision"))

  def ofEpochMilli(epochMilli: Long): Option[Timestamp] =
    fromInstant(Instant.ofEpochMilli(epochMilli))

  private def formatter(iso: Boolean): DateTimeFormatter = {
    val builder =
      new DateTimeFormatterBuilder()
        .append(DateTimeFormatter.ISO_LOCAL_DATE)
        .appendLiteral(if (iso) then 'T' else ' ')
        .appendPattern("HH:mm:ss")
        .appendFraction(ChronoField.NANO_OF_SECOND, 0, 6, true)

    (if (iso) builder.appendLiteral('Z') else builder).toFormatter(Locale.US)
  }

  val Formatter: DateTimeFormatter =
    formatter(iso = false)

  private val IsoFormatter: DateTimeFormatter =
    formatter(iso = true)

  def parse(s: String): Either[String, Timestamp] =
    Either
      .catchOnly[DateTimeParseException](LocalDateTime.parse(s, Formatter).toInstant(UTC))
      .orElse(Either.catchOnly[DateTimeParseException](LocalDateTime.parse(s, IsoFormatter).toInstant(UTC)))
      .leftMap(_ => s"Could not parse as a Timestamp: $s")
      .flatMap(fromInstant(_).toRight(s"Invalid Timestamp: $s"))

  extension (timestamp: Timestamp) {

    def format: String =
      Formatter.format(toLocalDateTime)

    def isoFormat: String =
      IsoFormatter.format(toLocalDateTime)

    def toInstant: Instant =
      timestamp

    def toLocalDateTime: LocalDateTime =
      LocalDateTime.ofInstant(timestamp, UTC)

    /** Gets the number of seconds from the Java epoch of 1970-01-01T00:00:00Z. */
    def epochSecond: Long =
      timestamp.getEpochSecond

    /**
     * Gets the number of microseconds after the start of the second returned
     * by `epochSecond`.
     */
    def µs: Long =
      timestamp.getNano / 1000L

    /**
     *  Converts this instant to the number of milliseconds from the epoch of
     * 1970-01-01T00:00:00Z.
     */
    def toEpochMilli: Long =
      timestamp.toEpochMilli

    def plusMillisOption(millisToAdd: Long): Option[Timestamp] =
      fromInstant(timestamp.plusMillis(millisToAdd))

    def plusMicrosOption(microsToAdd: Long): Option[Timestamp] =
      fromInstant(timestamp.plusNanos(microsToAdd * 1000))

    def plusSecondsOption(secondsToAdd: Long): Option[Timestamp] =
      fromInstant(timestamp.plusSeconds(secondsToAdd))
  }

  val FromString: Format[String, Timestamp] =
    Format(parse(_).toOption, _.format)

  val FromInstant: Prism[Instant, Timestamp] =
    Prism(fromInstant)(toInstant)

  val FromLocalDateTime: Prism[LocalDateTime, Timestamp] =
    Prism(fromLocalDateTime)(toLocalDateTime)

  given orderTimestamp: Order[Timestamp] with
    def compare(t0: Timestamp, t1: Timestamp): Int =
      t0.compareTo(t1)

  given decoderTimestamp: Decoder[Timestamp] =
    Decoder.decodeString.emap(parse)

  given encoderTimestamp: Encoder[Timestamp] =
    Encoder.encodeString.contramap[Timestamp](_.format)

}