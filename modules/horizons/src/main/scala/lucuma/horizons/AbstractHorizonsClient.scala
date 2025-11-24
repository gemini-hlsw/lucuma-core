// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.data.EitherT
import cats.effect.Temporal
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.data.PerSite
import lucuma.core.enums.Site
import lucuma.core.model.EphemerisKey

import java.time.Instant

import HorizonsConstants.*

private[horizons] abstract class AbstractHorizonsClient[F[_]: Temporal] extends HorizonsClient[F]:

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


