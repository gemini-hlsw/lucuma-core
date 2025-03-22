// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.*
import fs2.*
import fs2.data.xml.*
import lucuma.catalog.*
import lucuma.core.model.Target
import org.http4s.Uri
import org.http4s.syntax.all.*

object CatalogSearch {

  /**
   * Takes a name query and builds a uri to query simbad
   */
  def simbadSearchQuery[F[_]](query: QueryByName): Uri = {
    val simbadUri = uri"http://simbad.cfa.harvard.edu/simbad/sim-id"
    val base      = query.proxy.fold(simbadUri)(p =>
      Uri
        .fromString(s"${p}/$simbadUri")
        .getOrElse(sys.error("Cannot build gaia url"))
    )

    base
      .withQueryParam("output.format", "VOTable")
      .withQueryParam("Ident", query.id.value)
  }

  /**
   * Takes a search query and builds a uri to query gaia
   */
  def gaiaSearchUri[F[_]](
    query: ADQLQuery
  )(using CatalogAdapter.Gaia, ADQLInterpreter): Uri =
    gaiaSearchByQueryString(ADQLGaiaQuery.adql(summon[CatalogAdapter.Gaia], query), query.proxy)

  /**
   * Takes a source id and builds a uri to query gaia for that one star.
   */
  def gaiaSearchUriById[F[_]](
    id:    Long,
    proxy: Option[Uri] = None
  )(using CatalogAdapter.Gaia): Uri =
    gaiaSearchByQueryString(GaiaSourceIdQuery.idQueryString(id), proxy)

  /**
   * Helper method for the gaia queries.
   */
  private def gaiaSearchByQueryString[F[_]](query: String, proxy: Option[Uri]): Uri = {
    val esaUri = uri"https://gea.esac.esa.int/tap-server/tap/sync"
    val base   = proxy.fold(esaUri)(p =>
      Uri
        .fromString(s"${p}/$esaUri")
        .getOrElse(sys.error("Cannot build gaia url"))
    )
    base
      .withQueryParam("REQUEST", "doQuery")
      .withQueryParam("LANG", "ADQL")
      .withQueryParam("FORMAT", "votable_plain")
      .withQueryParam("QUERY", query)
  }

  /**
   * FS2 pipe to convert a stream of String to targets
   */
  def siderealTargets[F[_]: RaiseThrowable](
    adapter: CatalogAdapter
  ): Pipe[F, String, EitherNec[CatalogProblem, CatalogTargetResult]] =
    in =>
      in.flatMap(Stream.emits(_))
        .through(events[F, Char]())
        .through(normalize[F])
        .through(VoTableParser.xml2targets[F](adapter))

  /**
   * FS2 pipe to convert a stream of String to guide stars
   */
  def guideStars[F[_]: RaiseThrowable](
    adapter: CatalogAdapter
  ): Pipe[F, String, EitherNec[CatalogProblem, Target.Sidereal]] =
    in =>
      in.flatMap(Stream.emits(_))
        .through(events[F, Char]())
        .through(normalize[F])
        .through(VoTableParser.xml2guidestars[F](adapter))
}
