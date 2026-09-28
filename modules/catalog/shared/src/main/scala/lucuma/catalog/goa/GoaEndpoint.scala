// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.syntax.eq.*
import lucuma.core.util.Enumerated
import monocle.Optional
import org.http4s.Uri

/**
 * The GOA endpoint a query targets: the JSON API or the browsable search page for the same
 * selection.
 */
enum GoaEndpoint(val tag: String, val path: String) derives Enumerated:
  case JsonSummary extends GoaEndpoint("json_summary", "jsonsummary")
  case SearchForm  extends GoaEndpoint("search_form", "searchform")

object GoaEndpoint:

  /**
   * The endpoint of a query URL built by `GoaParams.toUri`, whatever its base. Replacing it
   * retargets the same selection; a URL that is not a GOA query is left alone.
   */
  val fromUri: Optional[Uri, GoaEndpoint] =
    Optional[Uri, GoaEndpoint](uri =>
      GoaQueryPath.endpointIndex(uri).flatMap(i => fromPath(uri.path.segments(i).encoded))
    )(endpoint =>
      uri =>
        GoaQueryPath
          .endpointIndex(uri)
          .fold(uri)(i => GoaQueryPath.replaceSegment(uri, i, endpoint.path))
    )

  private[goa] def fromPath(path: String): Option[GoaEndpoint] =
    values.find(_.path === path)

private[goa] object GoaQueryPath:

  private[goa] val Filter: List[String] = List("notengineering", "NotFail")

  /** Index of the endpoint segment: the one right before the fixed filter segments. */
  def endpointIndex(uri: Uri): Option[Int] =
    val segs = uri.path.segments.map(_.encoded)
    segs.indices.find: i =>
      GoaEndpoint.fromPath(segs(i)).isDefined && segs.slice(i + 1, i + 1 + Filter.size) == Filter

  def instrumentSegment(uri: Uri): Option[String] =
    endpointIndex(uri).flatMap(i => uri.path.segments.lift(i + 1 + Filter.size).map(_.decoded()))

  def replaceSegment(uri: Uri, i: Int, value: String): Uri =
    uri.withPath(
      Uri.Path(
        uri.path.segments.updated(i, Uri.Path.Segment(value)),
        absolute = uri.path.absolute,
        endsWithSlash = uri.path.endsWithSlash
      )
    )
