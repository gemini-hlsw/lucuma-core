// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.effect.Concurrent
import cats.syntax.all.*
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client

object SEDDataLoader:
  def load[F[_]: Concurrent](httpClient: Client[F], baseUrl: Uri): F[SEDDataConfig] =
    for
      starsContent   <- fetchFile(httpClient, baseUrl / "match_sed_stars.dat")
      gravityContent <- fetchFile(httpClient, baseUrl / "match_sed_log_g.csv")
      stars          <- SEDDataParsers.parseStarsFile(starsContent).liftTo[F]
      gravity        <- SEDDataParsers.parseGravityFile(gravityContent).liftTo[F]
    yield SEDDataConfig(stars, gravity)

  def loadMatcher[F[_]: Concurrent](httpClient: Client[F], baseUrl: Uri): F[SEDMatcher] =
    load(httpClient, baseUrl).map(SEDMatcher.fromConfig)

  private def fetchFile[F[_]: Concurrent](client: Client[F], uri: Uri): F[String] =
    client.expect[String](Request[F](uri = uri))
