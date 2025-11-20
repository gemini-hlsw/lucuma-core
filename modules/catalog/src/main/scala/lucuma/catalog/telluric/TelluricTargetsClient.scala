// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import cats.Applicative
import cats.data.EitherNec
import cats.effect.Concurrent
import cats.syntax.all.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.syntax.*
import fs2.text
import io.circe.syntax.*
import lucuma.catalog.csv.ImportProblem
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogSearch
import lucuma.catalog.votable.QueryByName
import lucuma.core.model.Target
import org.http4s.Method.*
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

/**
 * Client for the telluric targets service
 */
trait TelluricTargetsClient[F[_]]:
  def search(input: TelluricSearchInput): F[List[TelluricStar]]

  def searchWithSiderealTargets(
    input: TelluricSearchInput,
    proxy: Option[Uri] = None
  ): F[List[EitherNec[ImportProblem, (TelluricStar, Target.Sidereal)]]]

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

          override def searchWithSiderealTargets(
            input: TelluricSearchInput,
            proxy: Option[Uri] = None
          ): F[List[EitherNec[ImportProblem, (TelluricStar, Target.Sidereal)]]] =
            for {
              telluricStars <- search(input)
              results       <- telluricStars.traverse { star =>
                                 val queryUri =
                                   CatalogSearch.simbadSearchQuery(QueryByName(star.simbadName, proxy))
                                 val request  = Request[F](GET, queryUri)

                                 client
                                   .stream(request)
                                   .flatMap(
                                     _.body
                                       .through(text.utf8.decode)
                                       .through(CatalogSearch.siderealTargets[F](CatalogAdapter.Simbad))
                                   )
                                   .compile
                                   .toList
                                   .map(
                                     _.map(
                                       _.leftMap(e =>
                                         ImportProblem.LookupError(e.foldMap(_.displayValue), None)
                                       )
                                     )
                                   )
                                   .map { imports =>
                                     if (imports.length === 1)
                                       imports.head.map(result => (star, result.target)).toEitherNec
                                     else
                                       ImportProblem
                                         .LookupError(s"Multiple or no matches for ${star.simbadName}",
                                                      None
                                         )
                                         .leftNec
                                   }
                                   .handleError { e =>
                                     ImportProblem
                                       .LookupError(e.getMessage, None)
                                       .leftNec
                                   }
                               }
            } yield results

  def noop[F[_]: Applicative]: TelluricTargetsClient[F] = new TelluricTargetsClient[F]:
    def search(input: TelluricSearchInput): F[List[TelluricStar]] =
      List.empty.pure[F]

    def searchWithSiderealTargets(
      input:      TelluricSearchInput,
      httpClient: Client[F],
      proxy:      Option[Uri] = None
    ): F[List[EitherNec[ImportProblem, (TelluricStar, Target.Sidereal)]]] =
      List.empty.pure[F]
