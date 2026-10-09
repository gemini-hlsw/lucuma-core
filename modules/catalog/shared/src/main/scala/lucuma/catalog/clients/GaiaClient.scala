// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.Applicative
import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.GaiaStellarParameters
import lucuma.catalog.votable.*
import lucuma.core.model.Target
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.otel4s.trace.Tracer

trait GaiaClient[F[_]]:
  /**
   * Request and parse data from Gaia
   */
  def query(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]]

  /**
   * Request and parse data from Gaia for a single source.
   */
  def queryById(sourceId: Long): F[EitherNec[CatalogProblem, CatalogTargetResult]]

  /**
   * Request and parse data from Gaia for guide stars.
   */
  def queryGuideStars(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]]

  /**
   * Request and parse data from Gaia for a single guide star source.
   */
  def queryByIdGuideStar(sourceId: Long): F[EitherNec[CatalogProblem, Target.Sidereal]]

  /**
   * Effective temperature and surface gravity of the given sources, ESP-HS where Gaia has it and
   * GSP-Phot otherwise. Sources without values are absent. Empty when no configured adapter can
   * look them up (GAVO cannot; DataLab has GSP-Phot only).
   */
  def queryStellarParameters(sourceIds: List[Long]): F[Map[Long, GaiaStellarParameters]]

object GaiaClient:
  inline def build[F[_]: Concurrent: Tracer: LoggerFactory](
    httpClient: Client[F],
    modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
    adapters:   NonEmptyChain[CatalogAdapter.Gaia] = DefaultAdapters
  ) = GaiaClientImpl[F](httpClient, modUri, adapters)

  def noop[F[_]: Applicative]: GaiaClient[F] = new GaiaClient[F]:
    def query(adqlQuery: ADQLQuery)(using
      ADQLInterpreter
    ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
      List.empty.pure

    def queryById(sourceId: Long): F[EitherNec[CatalogProblem, CatalogTargetResult]] =
      CatalogProblem.SourceIdNotFound(sourceId).leftNec.pure[F]

    def queryGuideStars(adqlQuery: ADQLQuery)(using
      ADQLInterpreter
    ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
      List.empty.pure

    def queryByIdGuideStar(sourceId: Long): F[EitherNec[CatalogProblem, Target.Sidereal]] =
      CatalogProblem.SourceIdNotFound(sourceId).leftNec.pure[F]

    def queryStellarParameters(sourceIds: List[Long]): F[Map[Long, GaiaStellarParameters]] =
      Map.empty.pure

  val DefaultAdapters: NonEmptyChain[CatalogAdapter.Gaia] =
    NonEmptyChain.of(
      CatalogAdapter.Gaia3DataLab,
      CatalogAdapter.Gaia3LiteGavo,
      CatalogAdapter.Gaia3EsaProxy
    )
