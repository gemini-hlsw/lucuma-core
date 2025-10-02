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
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client

// TODO Add Trace, we need natchez
class GaiaClientImpl[F[_]: Concurrent](
  httpClient: Client[F],
  modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
  adapters:   NonEmptyChain[CatalogAdapter.Gaia] = GaiaClient.DefaultAdapters
) extends GaiaClient[F] {

  /**
   * Request and parse data from Gaia (full target data with all magnitude bands + angular size).
   */
  def query(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
    multiAdapterQueryFullTargets(queryUri(_, adqlQuery))

  /**
   * Request and parse data from Gaia for a single source (full target data).
   */
  def queryById(sourceId: Long): F[EitherNec[CatalogProblem, CatalogTargetResult]] =
    multiAdapterQueryFullTargets(queryUriById(_, sourceId)).map:
      _.headOption.toRight(NonEmptyChain(CatalogProblem.SourceIdNotFound(sourceId))).flatten

  /**
   * Request and parse data from Gaia for guide stars (filtered to single magnitude band).
   */
  def queryGuideStars(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    multiAdapterQueryGuideStars(queryUri(_, adqlQuery))

  /**
   * Request and parse data from Gaia for a single guide star source.
   */
  def queryByIdGuideStar(sourceId: Long): F[EitherNec[CatalogProblem, Target.Sidereal]] =
    multiAdapterQueryGuideStars(queryUriById(_, sourceId)).map:
      _.headOption.toRight(NonEmptyChain(CatalogProblem.SourceIdNotFound(sourceId))).flatten

  private def multiAdapterQueryFullTargets(
    queryUri: CatalogAdapter.Gaia => Uri
  ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
    adapters.map(adapter => queryGaiaTargets(adapter, queryUri(adapter))).raceAllToSuccess

  private def multiAdapterQueryGuideStars(
    queryUri: CatalogAdapter.Gaia => Uri
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    adapters.map(adapter => queryGaiaGuideStars(adapter, queryUri(adapter))).raceAllToSuccess

  private def queryGaiaTargets(
    adapter:  CatalogAdapter.Gaia,
    queryUri: Uri
  ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
    val request: Request[F] = Request[F](Method.GET, modUri(queryUri))
    httpClient
      .stream(request)
      .flatMap:
        _.body
          .through(fs2.text.utf8.decode)
          .through(CatalogSearch.siderealTargets(adapter))
      .compile
      .toList

  private def queryGaiaGuideStars(
    adapter:  CatalogAdapter.Gaia,
    queryUri: Uri
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    val request: Request[F] = Request[F](Method.GET, modUri(queryUri))
    httpClient
      .stream(request)
      .flatMap:
        _.body
          .through(fs2.text.utf8.decode)
          .through(CatalogSearch.guideStars(adapter))
      .compile
      .toList

  /**
   * Takes a search query and builds a uri to query gaia.
   */
  private def queryUri(adapter: CatalogAdapter.Gaia, query: ADQLQuery)(using
    intepreter: ADQLInterpreter
  ): Uri =
    buildQueryUri(
      adapter.uri,
      intepreter.buildQueryString(adapter, query),
      adapter.format
    )

  /**
   * Takes a source id and builds a uri to query gaia for that one star.
   */
  private def queryUriById[F[_]](adapter: CatalogAdapter.Gaia, sourceId: Long): Uri =
    buildQueryUri(
      adapter.uri,
      idQueryString(adapter, sourceId),
      adapter.format
    )

  private def idQueryString(adapter: CatalogAdapter.Gaia, sourceId: Long): String =
    val fields = adapter.allFields.map(_.id.value.toLowerCase).mkString(",")
    f"""|SELECT $fields
      |     FROM ${adapter.gaiaDB}
      |     WHERE source_id = $sourceId
    """.stripMargin

  /**
   * Helper method for the gaia queries.
   */
  private def buildQueryUri(
    base:   Uri,
    query:  String,
    format: String
  ): Uri =
    base
      .withQueryParam("REQUEST", "doQuery")
      .withQueryParam("LANG", "ADQL")
      .withQueryParam("FORMAT", format)
      .withQueryParam("QUERY", query)
}
