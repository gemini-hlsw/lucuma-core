// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.RaiseThrowable
import lucuma.catalog.BlindOffsetCandidate
import lucuma.catalog.votable.*
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Target
import lucuma.core.syntax.all.ToIntOps
import lucuma.core.syntax.effect.raceAllToSuccess
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client

import java.time.Instant

// TODO Add Trace, we need natchez
class GaiaClientImpl[F[_]](
  httpClient: Client[F],
  modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
  adapters:   NonEmptyChain[CatalogAdapter.Gaia] = GaiaClient.DefaultAdapters
)(using F: Concurrent[F])
    extends GaiaClient[F] {

  /**
   * Request and parse data from Gaia
   */
  def query(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    multiAdapterQuery(queryUri(_, adqlQuery))

  /**
   * Request and parse data from Gaia for a single source.
   */
  def queryById(sourceId: Long): F[EitherNec[CatalogProblem, Target.Sidereal]] =
    multiAdapterQuery(queryUriById(_, sourceId)).map:
      _.headOption.toRight(NonEmptyChain(CatalogProblem.SourceIdNotFound(sourceId))).flatten

  /**
   * Get all blind offset star candidates within 180 arcseconds of the base coordinate with G
   * magnitude > 12, sorted by score (best first), accounting for proper motion and other
   * time-dependent effects at the specified observation time.
   */
  def blindOffsetCandidates(
    baseTracking:    ObjectTracking,
    observationTime: Instant
  ): F[List[BlindOffsetCandidate]] =
    baseTracking.at(observationTime) match {
      case Some(baseCoords) =>
        val baseCoordinates = baseCoords.value
        val searchRadius    = 180.arcseconds
        val adqlQuery       = QueryByADQL(
          base = baseCoordinates,
          shapeConstraint = ShapeExpression.centeredEllipse(searchRadius * 2, searchRadius * 2),
          brightnessConstraints = None
        )

        println(adqlQuery)

        val interpreter = ADQLInterpreter.blindOffsetCandidates(using summon[ShapeInterpreter])

        query(adqlQuery)(using interpreter)
          .map(_.collect { case Right(target) => target })
          .map(
            BlindOffsetCandidate.sortCandidatesFromTargets(_, baseTracking, observationTime)
          )
      case None             =>
        F.pure(List.empty[BlindOffsetCandidate])
    }

  private def multiAdapterQuery(
    queryUri: CatalogAdapter.Gaia => Uri
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    adapters.map(adapter => queryGaiaSource(adapter, queryUri(adapter))).raceAllToSuccess

  private def queryGaiaSource(
    adapter:  CatalogAdapter.Gaia,
    queryUri: Uri
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    println(queryUri)
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
