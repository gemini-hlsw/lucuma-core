// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.RaiseThrowable
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.votable.*
import lucuma.core.model.Target
import lucuma.core.syntax.effect.raceAllToSuccess
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.ci.CIString

// TODO Add Trace, we need natchez
class GaiaClientImpl[F[_]: Concurrent](
  httpClient: Client[F],
  modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
  adapters:   NonEmptyChain[CatalogAdapter.Gaia] = GaiaClient.DefaultAdapters
) extends GaiaClient[F] {

  /**
   * Request and parse data from Gaia.
   */
  def query(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
    multiAdapterQuery(queryUri(_, adqlQuery), CatalogSearch.siderealTargets)

  /**
   * Request and parse data from Gaia for a single source.
   */
  def queryById(sourceId: Long): F[EitherNec[CatalogProblem, CatalogTargetResult]] =
    multiAdapterQuery(queryUriById(_, sourceId), CatalogSearch.siderealTargets).map:
      _.headOption.toRight(NonEmptyChain(CatalogProblem.SourceIdNotFound(sourceId))).flatten

  /**
   * Request and parse data from Gaia for guide stars.
   */
  def queryGuideStars(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    multiAdapterQuery(queryUri(_, adqlQuery), CatalogSearch.guideStars)

  /**
   * Request and parse data from Gaia for a single guide star source.
   */
  def queryByIdGuideStar(sourceId: Long): F[EitherNec[CatalogProblem, Target.Sidereal]] =
    multiAdapterQuery(queryUriById(_, sourceId), CatalogSearch.guideStars).map:
      _.headOption.toRight(NonEmptyChain(CatalogProblem.SourceIdNotFound(sourceId))).flatten

  private def multiAdapterQuery[A](
    queryUri: CatalogAdapter.Gaia => Uri,
    parser:   CatalogAdapter.Gaia => fs2.Pipe[F, String, EitherNec[CatalogProblem, A]]
  ): F[List[EitherNec[CatalogProblem, A]]] =
    adapters
      .map(adapter => queryGaia(queryUri(adapter), headersFor(adapter), parser(adapter)))
      .raceAllToSuccess

  private def queryGaia[A](
    queryUri: Uri,
    headers:  Headers,
    parser:   fs2.Pipe[F, String, EitherNec[CatalogProblem, A]]
  ): F[List[EitherNec[CatalogProblem, A]]] =
    val request: Request[F] = Request[F](Method.GET, modUri(queryUri), headers = headers)
    httpClient
      .stream(request)
      .flatMap:
        _.body
          .through(fs2.text.utf8.decode)
          .through(parser)
      .compile
      .toList

  private def headersFor(adapter: CatalogAdapter.Gaia): Headers =
    Headers(adapter.requestHeaders.map((k, v) => org.http4s.Header.Raw(CIString(k), v)).toList)

  /**
   * Takes a search query and builds a uri to query gaia.
   */
  private def queryUri(adapter: CatalogAdapter.Gaia, query: ADQLQuery)(using
    intepreter: ADQLInterpreter
  ): Uri =
    adapter.buildQueryUri(intepreter.buildQueryString(adapter, query))

  /**
   * Takes a source id and builds a uri to query gaia for that one star.
   */
  private def queryUriById(adapter: CatalogAdapter.Gaia, sourceId: Long): Uri =
    adapter.buildQueryUri(idQueryString(adapter, sourceId))

  private def idQueryString(adapter: CatalogAdapter.Gaia, sourceId: Long): String =
    val fields = adapter.allFields.map(_.id.value.toLowerCase).mkString(",")
    f"""|SELECT $fields
        |     FROM ${adapter.gaiaDB}
        |     WHERE source_id = $sourceId
    """.stripMargin
}
