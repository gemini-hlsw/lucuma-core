// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.Applicative
import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogProblem
import lucuma.catalog.votable.CatalogSearch
import lucuma.core.syntax.effect.raceAllToSuccess
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client

trait SimbadClient[F[_]]:
  /**
   * Search SIMBAD for an exact match
   */
  def search(name: NonEmptyString): F[EitherNec[CatalogProblem, CatalogTargetResult]]

  /**
   * Search SIMBAD with optional wildcard support and result limit
   */
  def search(
    term:       NonEmptyString,
    wildcard:   Boolean,
    maxResults: Option[Int]
  ): F[List[CatalogTargetResult]]

object SimbadClient:
  val SimbadStrasbourg: Uri = Uri.unsafeFromString("http://simbad.u-strasbg.fr/simbad/sim-id")
  val SimbadHarvard: Uri    = Uri.unsafeFromString("http://simbad.cfa.harvard.edu/simbad/sim-id")

  val DefaultEndpoints: NonEmptyChain[Uri] =
    NonEmptyChain.of(SimbadStrasbourg, SimbadHarvard)

  def build[F[_]: Concurrent](
    httpClient: Client[F],
    modUri:     Uri => Uri = identity,
    endpoints:  NonEmptyChain[Uri] = DefaultEndpoints
  ): SimbadClient[F] = new SimbadClient[F]:

    def search(name: NonEmptyString): F[EitherNec[CatalogProblem, CatalogTargetResult]] =
      multiEndpointQuery(buildLookupUri(_, name)).map: results =>
        results.headOption match
          case Some(result) => result
          case None         => CatalogProblem.GenericError(s"No results for $name").leftNec

    def search(
      term:       NonEmptyString,
      wildcard:   Boolean,
      maxResults: Option[Int]
    ): F[List[CatalogTargetResult]] =
      multiEndpointQuery(buildSearchUri(_, term, wildcard, maxResults)).map: results =>
        results.collect { case Right(r) => r }

    private def multiEndpointQuery(
      buildUri: Uri => Uri
    ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
      endpoints
        .map(endpoint => querySimbad(buildUri(endpoint)))
        .raceAllToSuccess

    private def querySimbad(
      queryUri: Uri
    ): F[List[EitherNec[CatalogProblem, CatalogTargetResult]]] =
      val request: Request[F] = Request[F](Method.GET, modUri(queryUri))
      httpClient
        .stream(request)
        .flatMap:
          _.body
            .through(fs2.text.utf8.decode)
            .through(CatalogSearch.siderealTargets[F](CatalogAdapter.Simbad))
        .compile
        .toList

    private def buildLookupUri(base: Uri, name: NonEmptyString): Uri =
      base
        .withQueryParam("output.format", "VOTable")
        .withQueryParam("Ident", name.value)

    private def buildSearchUri(
      base:       Uri,
      term:       NonEmptyString,
      wildcard:   Boolean,
      maxResults: Option[Int]
    ): Uri =
      val uri = base
        .withQueryParam("output.format", "VOTable")
        .withQueryParam("Ident", term.value)

      val withMax = maxResults.fold(uri)(max => uri.withQueryParam("output.max", max.toString))
      if (wildcard) withMax.withQueryParam("NbIdent", "wild")
      else withMax

  def noop[F[_]: Applicative]: SimbadClient[F] = new SimbadClient[F]:
    def search(name: NonEmptyString): F[EitherNec[CatalogProblem, CatalogTargetResult]] =
      CatalogProblem.GenericError(s"No results for $name").leftNec.pure[F]

    def search(
      term:       NonEmptyString,
      wildcard:   Boolean,
      maxResults: Option[Int]
    ): F[List[CatalogTargetResult]] =
      List.empty.pure[F]
