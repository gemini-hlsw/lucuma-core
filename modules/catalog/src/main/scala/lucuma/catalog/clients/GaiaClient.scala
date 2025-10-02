// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.votable.*
import lucuma.core.model.Target
import org.http4s.Uri
import org.http4s.client.Client

trait GaiaClient[F[_]]:
  /**
   * Request and parse data from Gaia (full target data with all magnitude bands + angular size).
   */
  def query(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]]

  /**
   * Request and parse data from Gaia for a single source (full target data).
   */
  def queryById(sourceId: Long): F[EitherNec[CatalogProblem, CatalogTargetResult]]

  /**
   * Request and parse data from Gaia for guide stars (filtered to single magnitude band).
   */
  def queryGuideStars(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]]

  /**
   * Request and parse data from Gaia for a single guide star source.
   */
  def queryByIdGuideStar(sourceId: Long): F[EitherNec[CatalogProblem, Target.Sidereal]]

object GaiaClient:
  inline def build[F[_]: Concurrent](
    httpClient: Client[F],
    modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
    adapters:   NonEmptyChain[CatalogAdapter.Gaia] = DefaultAdapters
  ) = GaiaClientImpl[F](httpClient, modUri, adapters)

  val DefaultAdapters: NonEmptyChain[CatalogAdapter.Gaia] =
    NonEmptyChain.of(
      CatalogAdapter.Gaia3LiteGavo,
      CatalogAdapter.Gaia3LiteEsaProxy
    )
