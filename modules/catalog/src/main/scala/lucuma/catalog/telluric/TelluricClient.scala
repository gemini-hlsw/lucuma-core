// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import cats.effect.Async
import cats.syntax.all.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import io.circe.syntax.*
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

/**
 * Client for calling the telluric targets service
 */
trait TelluricClient[F[_]]:
  def search(input: TelluricSearchInput): F[List[TelluricStar]]

object TelluricClient:
  def apply[F[_]](using ev: TelluricClient[F]): TelluricClient[F] = ev

  def create[F[_]: Async: Logger](
    uri: Uri,
    client: Client[F]
  ): F[TelluricClient[F]] =
    given Http4sHttpBackend[F] = Http4sHttpBackend[F](client)
    for
      http <- Http4sHttpClient.of[F, Unit](uri)
    yield new TelluricClient[F]:
      override def search(input: TelluricSearchInput): F[List[TelluricStar]] =
        val encodedVars = TelluricSearchQuery.varEncoder(input).asJson
        for
          _ <- Logger[F].info(s"Telluric endpoint: $uri")
          _ <- Logger[F].info(s"GraphQL query: ${TelluricSearchQuery.document}")
          _ <- Logger[F].info(s"Query variables (encoded): ${encodedVars.spaces2}")
          response <- http
            .request(TelluricSearchQuery)
            .withInput(input)
          _ <- Logger[F].info(s"GraphQL response (raw): $response")
          result <- (response.data, response.errors) match
            case (Some(data), _) =>
              for
                _ <- Logger[F].info(s"GraphQL data (JSON):\n${data.asJson.spaces2}")
                _ <- Logger[F].info(s"Telluric search found ${data.size} candidates")
              yield data
            case (None, Some(errors)) =>
              for
                _ <- Logger[F].info(s"GraphQL errors: ${errors.toList.mkString("\n")}")
                // Server bug: when no results are found, it tries to add a column to an empty table
                isEmptyResultError = errors.toList.exists(_.message.contains("Empty table cannot have column set to scalar value"))
                result <- if (isEmptyResultError) {
                  Logger[F].info("No telluric candidates found (server returned empty table error)").as(List.empty[TelluricStar])
                } else {
                  Logger[F].error(s"GraphQL errors: $errors") *>
                  Async[F].raiseError[List[TelluricStar]](new Exception(s"GraphQL errors: $errors"))
                }
              yield result
            case (None, None) =>
              Logger[F].error("No data and no errors in GraphQL response") *>
              Async[F].raiseError[List[TelluricStar]](new Exception("No data and no errors in GraphQL response"))
        yield result
