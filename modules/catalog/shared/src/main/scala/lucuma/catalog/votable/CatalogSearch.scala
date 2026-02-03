// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

  /**
   * FS2 pipe to convert a stream of String to targets
   */
  def siderealTargets[F[_]: RaiseThrowable](
    adapter: CatalogAdapter
  ): Pipe[F, String, EitherNec[CatalogProblem, CatalogTargetResult]] =
    in =>
      in.flatMap(Stream.emits(_))
        .through(xml.events[F, Char]())
        .through(xml.normalize[F])
        .through(VoTableParser.xml2targets[F](adapter))

  /**
   * FS2 pipe to convert a stream of String to guide stars
   */
  def guideStars[F[_]: RaiseThrowable](
    adapter: CatalogAdapter
  ): Pipe[F, String, EitherNec[CatalogProblem, Target.Sidereal]] =
    in =>
      in.flatMap(fs2.Stream.emits(_))
        .through(xml.events[F, Char]())
        .through(xml.normalize[F])
        .through(VoTableParser.xml2guidestars[F](adapter))
