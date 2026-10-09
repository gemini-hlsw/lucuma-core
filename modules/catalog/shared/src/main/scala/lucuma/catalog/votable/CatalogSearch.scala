// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.*
import fs2.*
import fs2.data.xml
import lucuma.catalog.*
import lucuma.core.model.Target
import org.http4s.Uri
import org.http4s.syntax.all.*

object CatalogSearch:

  /**
   * Takes a name query and builds a uri to query simbad
   */
  def simbadSearchQuery[F[_]](query: QueryByName): Uri = {
    val simbadUri = uri"http://simbad.u-strasbg.fr/simbad/sim-id"
    val base      = query.proxy.fold(simbadUri)(p =>
      Uri
        .fromString(s"${p}/$simbadUri")
        .getOrElse(sys.error("Cannot build gaia url"))
    )

    base
      .withQueryParam("output.format", "VOTable")
      .withQueryParam("Ident", query.id.value)
  }

  // Parses a VOTable document with the given row pipe
  private def votable[F[_]: RaiseThrowable, A](
    rows: Pipe[F, xml.XmlEvent, A]
  ): Pipe[F, String, A] =
    in =>
      in.flatMap(Stream.emits(_))
        .through(xml.events[F, Char]())
        .through(xml.normalize[F])
        .through(rows)

  /**
   * FS2 pipe to convert a stream of String to targets
   */
  def siderealTargets[F[_]: RaiseThrowable](
    adapter: CatalogAdapter
  ): Pipe[F, String, EitherNec[CatalogProblem, CatalogTargetResult]] =
    votable(VoTableParser.xml2targets[F](adapter))

  /**
   * FS2 pipe to convert a stream of String to guide stars
   */
  def guideStars[F[_]: RaiseThrowable](
    adapter: CatalogAdapter
  ): Pipe[F, String, EitherNec[CatalogProblem, Target.Sidereal]] =
    votable(VoTableParser.xml2guidestars[F](adapter))

  /**
   * FS2 pipe to convert a stream of String to stellar parameters keyed by Gaia source id
   */
  def stellarParameters[F[_]: RaiseThrowable](
    adapter: CatalogAdapter
  ): Pipe[F, String, EitherNec[CatalogProblem, (Long, GaiaStellarParameters)]] =
    votable(VoTableParser.xml2stellarParameters[F](adapter))
