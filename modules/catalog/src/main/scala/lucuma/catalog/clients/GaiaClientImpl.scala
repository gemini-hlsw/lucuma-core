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
import org.http4s.Header
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*

import java.net.URLDecoder

// TODO Add Trace, we need natchez
class GaiaClientImpl[F[_]: {Concurrent, LoggerFactory as LF}](
  httpClient: Client[F],
  modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
  adapters:   NonEmptyChain[CatalogAdapter.Gaia] = GaiaClient.DefaultAdapters
) extends GaiaClient[F] {
  private given Logger[F] = LF.getLoggerFromName("gaia-client")

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
      .map(adapter => queryGaia(adapter, queryUri(adapter), parser(adapter)))
      .raceAllToSuccess
      .flatMap: (adapter, results) =>
        info"Selected catalog: ${adapter.adapterName}" *> results.pure[F]

  private def queryGaia[A](
    adapter:  CatalogAdapter.Gaia,
    queryUri: Uri,
    parser:   fs2.Pipe[F, String, EitherNec[CatalogProblem, A]]
  ): F[(CatalogAdapter.Gaia, List[EitherNec[CatalogProblem, A]])] =
    val headers             = Headers(adapter.requestHeaders.map((x, y) => Header.Raw(x, y)).toList)
    val request: Request[F] = Request[F](Method.GET, modUri(queryUri), headers = headers)

    info"Querying catalog: ${adapter.adapterName}, uri: ${URLDecoder.decode(queryUri.renderString, "UTF-8")}" *>
      info"curl ${headers.headers.map(h => s"-H '${h.name}: ${h.value}'").mkString(" ")} '${queryUri.renderString}'" *>
      httpClient
        .stream(request)
        .flatMap:
          _.body
            .through(fs2.text.utf8.decode)
            .through(parser)
        .compile
        .toList
        .tupleLeft(adapter)

  /**
   * Takes a search query and builds a uri to query gaia.
   */
  private def queryUri(adapter: CatalogAdapter.Gaia, query: ADQLQuery)(using
    intepreter: ADQLInterpreter
  ): Uri =
    adapter.queryUri(intepreter.buildQueryString(adapter, query))

  /**
   * Takes a source id and builds a uri to query gaia for that one star.
   */
  private def queryUriById(adapter: CatalogAdapter.Gaia, sourceId: Long): Uri =
    adapter.queryUri(idQueryString(adapter, sourceId))

  private def idQueryString(adapter: CatalogAdapter.Gaia, sourceId: Long): String =
    val fields = adapter.allFields.map(_.id.value.toLowerCase).mkString(",")
    f"""|SELECT $fields
        |     FROM ${adapter.gaiaDB}
        |     WHERE source_id = $sourceId
    """.stripMargin
}
