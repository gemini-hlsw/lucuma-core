// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.effect.Temporal
import fs2.Stream
import fs2.text
import lucuma.core.data.PerSite
import lucuma.core.enums.Site
import lucuma.core.model.EphemerisKey
import org.http4s.Request
import org.http4s.Uri
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
    initialRetryInterval: FiniteDuration = 100.milli,
    modUri: Uri => Uri = identity // Override this if you need to add a CORS proxy
  ): HorizonsClient[F] =
    new AbstractHorizonsClient[F]:
      def stream(params: (String, String)*): Stream[F, String] =
        val uri = modUri(HorizonsUri).withQueryParams(params.toMap)
        def go(retriesRemaining: Int, interval: FiniteDuration): Stream[F, String] =
          client
            .stream:
              Request(uri = uri)
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

  def forTesting[F[_]: Temporal](
    fixture: Map[Set[(String, String)], String]
  ): HorizonsClient[F] =
    new AbstractHorizonsClient[F]:
      def stream(params: (String, String)*): Stream[F, String] =
        fixture.get(params.toSet) match
          case Some(s) => Stream.emits(s.linesIterator.toList)
          case None =>
            println(s"💫💫💫 HorizonsClient: Missing Fixture. Please add the following entry:")
            println()
            println:
              params
                .map(p => s"(\"${p._1}\", \"${p._2}\")")
                .mkString("key = Set(", ", ", ")")
            println(s"curl -k '${HorizonsUri.withQueryParams(params.toMap)}' | awk '{print \"|\" $$0}' | pbcopy")
            println()
            println(s"💫💫💫")
            throw new Error("Missing fixture.")
        
