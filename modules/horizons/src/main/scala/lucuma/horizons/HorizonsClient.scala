// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.data.EitherT
import cats.effect.Temporal
import cats.syntax.all.*
import fs2.Stream
import fs2.text
import lucuma.core.data.PerSite
import lucuma.core.enums.Site
import lucuma.core.model.EphemerisKey
import org.http4s.Headers
import org.http4s.Request
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.concurrent.duration.*

import HorizonsConstants.*

trait HorizonsClient[F[_]]:

  /**
   * Perform a raw Horizons request with the specified parameters, yielding the resulting
   * lines of text.
   */
  def stream(params: (String, String)*): Stream[F, String]

  /** Resolve a categorized search term to a list of candidates, which include the common name. */
  def resolve[A](search: HorizonsClient.Search[A]): F[Either[String, List[(A, String)]]]

  /** 
   * Select the ephemeris for the specified key, site, interval, and desired number of elements.
   * The number of elements returned will be approximately `elems`, with the caveat that there will
   * never be more than one element per minute.
   */
  def ephemeris(
    key: EphemerisKey.Horizons,
    site: Site,
    start: Instant,
    stop: Instant,
    elems: Int,
  ): F[Either[String, HorizonsEphemeris]]

  /** Equivalent to `ephemeris` but selects the ephemeris at both sites. */
  def ephemerisPerSite(
    key: EphemerisKey.Horizons,
    start: Instant,
    stop: Instant,
    elems: Int,
  ): F[Either[String, PerSite[HorizonsEphemeris]]]

  /**
   * Similar to `ephemeris` but selects elements starting at midnight UTC on the day of
   * `start` and advancing for `days` days at the specified cadence. The resulting ephemeris
   * will contain `days * cadence + 1` elements.
   */
  def alignedEphemeris(
    key: EphemerisKey.Horizons,
    site: Site,
    start: Instant,
    days: Int,
    cadence: HorizonsClient.ElementsPerDay,
  ): F[Either[String, HorizonsEphemeris]] =
    val aligned = 
      ZonedDateTime
        .ofInstant(start, ZoneOffset.UTC)
        .withHour(0)
        .withMinute(0)
        .withSecond(0)            
    ephemeris(key, site, aligned.toInstant, aligned.plusDays(days).toInstant, days * cadence)

  /** Equivalent to `alignedEphemeris` but selects the ephemeris at both sites. */
  def alignedEphemerisPerSite(
    key: EphemerisKey.Horizons,
    start: Instant,
    days: Int,
    cadence: HorizonsClient.ElementsPerDay,
  ): F[Either[String, PerSite[HorizonsEphemeris]]]

object HorizonsClient:

  /** Even divisors of 24 */
  type ElementsPerDay = 1 | 2 | 3 | 4 | 6 | 8 | 12 | 24

  enum Search[A](val queryString: String):
    case Comet(partial: String)     extends Search[EphemerisKey.Comet](s"NAME=$partial*;CAP")
    case Asteroid(partial: String)  extends Search[EphemerisKey.Asteroid](s"ASTNAM=$partial*")
    case MajorBody(partial: String) extends Search[EphemerisKey.MajorBody](s"$partial")

  /**
   * Construct a `HorizonsClient`. Requests will be retried automatically on failure, up to `maxRetries`,
   * with a waiting period of `initialRetryInterval` that doubles after each retry.
   */
  def apply[F[_]: Temporal: Logger](
    client: Client[F],
    maxRetries: Int = 5,
    initialRetryInterval: FiniteDuration = 100.milli
  ): HorizonsClient[F] =
    new HorizonsClient[F]:

      def stream(params: (String, String)*): Stream[F, String] =
        def go(retriesRemaining: Int, interval: FiniteDuration): Stream[F, String] =
          client
            .stream:
              Request(uri = HorizonsUri.withQueryParams(params.toMap))
            .flatMap: res =>
              if res.status.isSuccess then
                res
                  .body
                  .through(text.utf8.decode)
                  .through(text.lines)
              else
                if retriesRemaining > 0 then
                  Stream.exec(Logger[F].warn(s"Retrying HORIZONS query (retries remaining: ${retriesRemaining - 1})")) ++
                  Stream.exec(Temporal[F].sleep(interval)) ++
                  go(retriesRemaining - 1, interval * 2)
                else
                  Stream.raiseError(RuntimeException("HORIZONS service unavailable."))
        go(maxRetries, initialRetryInterval)

      def resolve[A](search: HorizonsClient.Search[A]): F[Either[String, List[(A, String)]]] =
        stream(
          Format    -> "text",
          Ephemeris -> No,
          Command   -> s"'${search.queryString}'"
        ).compile.toList.map:
          HorizonsParser.parseResponse(search, _)

      def ephemeris(
        key: EphemerisKey.Horizons,
        site: Site,
        start: Instant,
        stop: Instant,
        elems: Int,
      ): F[Either[String, HorizonsEphemeris]] =
        if !stop.isAfter(start) then Left("Stop must fall after start.").pure[F]
        else if elems < 1 then Left("Cannot select fewer than one element.").pure[F]
        else {
          val minutes  = (stop.toEpochMilli - start.toEpochMilli) / (1000L * 60)
          val stepSize = 1L max minutes / elems
          stream(
              Format         -> Text,
              Ephemeris      -> Yes,
              Center         -> CenterCoord,
              CoordType      -> CoordTypeGeo,
              Command        -> s"'${key.queryString}'",
              SiteCoord      -> horizonsCoordsAt(site),
              StartTime      -> s"'${HorizonsDateFormat.format(start)}'",
              StopTime       -> s"'${HorizonsDateFormat.format(stop)}'",
              StepSize       -> s"${stepSize}m",
              ExtraPrecision -> Yes,
              TimeDigits     -> FractionalSec,
              Quantities     -> "'1,3,8,9'" // see 3. Observer Table at https://ssd.jpl.nasa.gov/horizons/manual.html#output
          ) .dropThrough(_ != "$$SOE")
            .takeWhile(_ != "$$EOE")
            .map(HorizonsParser.parseEntry)
            .takeThrough(_.isRight) // stop on the first error, if any
            .compile
            .toList
            .map: lines =>
              lines.sequence.map: elements =>
                HorizonsEphemeris(
                  key, site, start, stop, elements
                )
        }
      
      def ephemerisPerSite(
        key: EphemerisKey.Horizons,
        start: Instant,
        stop: Instant,
        elems: Int,
      ): F[Either[String, PerSite[HorizonsEphemeris]]] =
        PerSite
          .unfoldF: site =>
            EitherT(ephemeris(key, site, start, stop, elems))
          .value

      def alignedEphemerisPerSite(
        key: EphemerisKey.Horizons,
        start: Instant,
        days: Int,
        cadence: HorizonsClient.ElementsPerDay,
      ): F[Either[String, PerSite[HorizonsEphemeris]]] =
        PerSite
          .unfoldF: site =>
            EitherT(alignedEphemeris(key, site, start, days, cadence))          
          .value

