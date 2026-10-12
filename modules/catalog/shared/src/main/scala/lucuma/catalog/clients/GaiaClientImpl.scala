// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import cats.effect.kernel.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.RaiseThrowable
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.GaiaStellarParameters
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
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.SpanFinalizer
import org.typelevel.otel4s.trace.StatusCode
import org.typelevel.otel4s.trace.Tracer

import java.net.URLDecoder

class GaiaClientImpl[F[_]: {Concurrent, Tracer as T, LoggerFactory as LF}](
  httpClient: Client[F],
  modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
  adapters:   NonEmptyChain[CatalogAdapter.Gaia] = GaiaClient.DefaultAdapters
) extends GaiaClient[F] {
  private given Logger[F] = LF.getLoggerFromName("gaia-client")

  // Catalog queries race multiple adapters and keep the fastest. A cancelled query is an
  // expected outcome, not a failure.
  private val racingSpanStrategy: SpanFinalizer.Strategy = { case Resource.ExitCase.Errored(e) =>
    SpanFinalizer.recordException(e) |+| SpanFinalizer.setStatus(StatusCode.Error)
  }

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

  // Adapters with ESP-HS are raced first; the others only answer when those fail or find nothing,
  // as their complete-looking answers would otherwise replace ESP-HS values with GSP-Phot.
  def queryStellarParameters(sourceIds: List[Long]): F[Map[Long, GaiaStellarParameters]] =
    sourceIds.distinct
      .grouped(GaiaClientImpl.MaxIdsPerQuery)
      .toList
      .parTraverseN(GaiaClientImpl.ParallelBatches)(lookupBatch)
      .map(_.foldLeft(Map.empty[Long, GaiaStellarParameters])(_ ++ _))

  private def lookupBatch(sourceIds: List[Long]): F[Map[Long, GaiaStellarParameters]] =
    val capable                  = adapters.toList.flatMap: adapter =>
      adapter.stellarParametersByIdQuery(sourceIds).map(q => (adapter, adapter.queryUri(q)))
    // Adapters sharing a server and a query would otherwise send the same request twice
    val (withEspHs, gspPhotOnly) = capable.distinctBy(_._2).partition(_._1.providesEspHs)
    List(withEspHs, gspPhotOnly).flatMap(raceStellarParameters) match
      case Nil           => Map.empty.pure
      case first :: rest =>
        rest
          .foldLeft(first)((found, next) => found.handleErrorWith(_ => next))
          .handleErrorWith: e =>
            warn"No stellar parameters found: ${e.getMessage}".as(Map.empty)

  // An attempt that finds nothing fails, so an error document or an empty reply does not win the
  // race: queryGaia does not check the HTTP status, and such replies parse to no rows.
  private def raceStellarParameters(
    group: List[(CatalogAdapter.Gaia, Uri)]
  ): Option[F[Map[Long, GaiaStellarParameters]]] =
    NonEmptyChain
      .fromSeq(group)
      .map:
        _.map: (adapter, uri) =>
          queryGaia(adapter, uri, CatalogSearch.stellarParameters(adapter)).flatMap: (_, results) =>
            val (problems, rows) = results.partitionEither(identity)
            val found            = rows.toMap
            val report           =
              warn"${adapter.adapterName}: ${problems.length} unreadable rows, e.g. ${problems.take(3).map(_.head).mkString("; ")}"
                .whenA(problems.nonEmpty)
            report *>
              (if found.isEmpty then
                 new RuntimeException(s"${adapter.adapterName} found no values").raiseError
               else (adapter, found).pure)
        .raceAllToSuccess
          .flatMap: (adapter, found) =>
            info"Selected catalog: ${adapter.adapterName}".as(found)

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

    T.spanBuilder("query gaia")
      .withFinalizationStrategy(racingSpanStrategy)
      .addAttribute(Attribute("adapter", adapter.adapterName))
      .build
      .surround:
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

object GaiaClientImpl:
  // 50 ids of up to 19 digits, comma-encoded, are about 1.1 KB, under the 2048-character URL
  // limit some proxies enforce even after modUri wraps the request
  val MaxIdsPerQuery: Int = 50

  val ParallelBatches: Int = 4
