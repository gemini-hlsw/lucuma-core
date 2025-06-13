// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import lucuma.catalog.votable.*
import lucuma.core.model.Target
import org.http4s.Uri
import org.http4s.client.Client

trait GaiaClient[F[_]]:
  /**
   * Request and parse data from Gaia.
   */
  def query(adqlQuery: ADQLQuery)(using
    ADQLInterpreter
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]]

  /**
   * Request and parse data from Gaia for a single source.
   */
  def queryById(sourceId: Long): F[EitherNec[CatalogProblem, Target.Sidereal]]

object GaiaClient:
  inline def build[F[_]](
    httpClient: Client[F],
    modUri:     Uri => Uri = identity, // Override this if you need to add a CORS proxy
    adapters:   NonEmptyChain[CatalogAdapter.Gaia] = DefaultAdapters
  )(using F: Concurrent[F]) =
    GaiaClientImpl[F](httpClient, modUri, adapters)

  val DefaultAdapters: NonEmptyChain[CatalogAdapter.Gaia] =
    NonEmptyChain.of(
      CatalogAdapter.Gaia3LiteGavo,
      CatalogAdapter.Gaia3LiteEsaProxy
    )
