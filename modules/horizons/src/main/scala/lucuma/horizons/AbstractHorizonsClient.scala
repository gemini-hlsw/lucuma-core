// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.effect.Temporal
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.data.PerSite
import lucuma.core.enums.Site
import lucuma.core.model.Ephemeris

import java.time.Instant

import HorizonsConstants. { Ephemeris as MkEphemeris, * }

private[horizons] abstract class AbstractHorizonsClient[F[_]: Temporal] extends HorizonsClient[F]:

  def resolve[A](search: HorizonsClient.Search[A]): F[Either[String, List[(A, String)]]] =
    stream(
      Format      -> "text",
      MkEphemeris -> No,
      Command     -> s"'${search.queryString}'"
    ).compile.toList.map:
      HorizonsParser.parseResponse(search, _)

  def ephemeris(
    key: Ephemeris.Key.Horizons,
    start: Instant,
    stop: Instant,
    elems: Int,
  ): F[Either[String, Ephemeris.Horizons]] =
    if !stop.isAfter(start) then Left("Stop must fall after start.").pure[F]
    else if elems < 1 then Left("Cannot select fewer than one element.").pure[F]
    else
      PerSite
      .unfoldF(fetch(key, _, start, stop, elems))
      .map: ps =>
        ps.sequence.map(Ephemeris.Horizons(key, start, stop, _))         
  
  private def fetch(
    key: Ephemeris.Key.Horizons,
    site: Site,
    start: Instant,
    stop: Instant,
    elems: Int,
  ): F[Either[String, List[Ephemeris.Horizons.Element]]] =
    if !stop.isAfter(start) then Left("Stop must fall after start.").pure[F]
    else if elems < 1 then Left("Cannot select fewer than one element.").pure[F]
    else {
      val minutes  = (stop.toEpochMilli - start.toEpochMilli) / (1000L * 60)
      val stepSize = 1L max minutes / elems
      stream(
          Format         -> Text,
          MkEphemeris    -> Yes,
          Center         -> horizonsSiteCode(site),
          Command        -> s"'${key.queryString}'",
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
          lines.sequence
    }
