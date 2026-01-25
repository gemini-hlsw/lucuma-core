// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.*
import cats.effect.Concurrent
import fs2.*
import lucuma.catalog.*
import lucuma.core.refined.auto.*
import org.http4s.Method.*
import org.http4s.Request
import org.http4s.client.Client

trait SimbadQuerySample:

  def simbadQuery[F[_]: Concurrent](
    client: Client[F]
  ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] = {
    val request = Request[F](GET, CatalogSearch.simbadSearchQuery(QueryByName("Vega".refined)))
    client
      .stream(request)
      .flatMap(
        _.body
          .through(text.utf8.decode)
          .through(CatalogSearch.siderealTargets[F](CatalogAdapter.Simbad()))
      )
      .compile
      .toList
  }
