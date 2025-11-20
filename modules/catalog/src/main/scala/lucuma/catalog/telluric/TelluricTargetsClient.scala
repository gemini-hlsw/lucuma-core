// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.syntax.*
import io.circe.syntax.*
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

/**
 * Client for the telluric targets service
 */
trait TelluricTargetsClient[F[_]]:
  def search(input: TelluricSearchInput): F[List[TelluricStar]]

object TelluricTargetsClient:
  def build[F[_]: Concurrent: Logger](
    uri:    Uri,
    client: Client[F]
  ): F[TelluricTargetsClient[F]] =
    given Http4sHttpBackend[F] = Http4sHttpBackend[F](client)

    Http4sHttpClient
      .of[F, Unit](uri)
      .map: http =>
        new TelluricTargetsClient[F]:
          override def search(input: TelluricSearchInput): F[List[TelluricStar]] =
            val encodedVars = TelluricSearchQuery.varEncoder(input).asJson

            for {
              _        <- debug"Telluric targets call: $uri ${encodedVars.noSpaces}"
              response <- http
                            .request(TelluricSearchQuery)
                            .withInput(input)
                            .raiseGraphQLErrors
              _        <- debug"GraphQL response: $response"
            } yield response

  def noop[F[_]: Applicative]: TelluricTargetsClient[F] = new TelluricTargetsClient[F]:
    def search(input: TelluricSearchInput): F[List[TelluricStar]] =
      List.empty.pure[F]
