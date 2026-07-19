// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.parser.decode
import lucuma.catalog.goa.syntax.*
import org.http4s.Header
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.ci.CIStringSyntax

import java.time.format.DateTimeFormatter

class GoaClientImpl[F[_]: Concurrent](
  httpClient: Client[F],
  baseUri:    Uri,
  modUri:     Uri => Uri
) extends GoaClient[F]:

  import codecs.given

  private val dateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyyMMdd")

  def query(params: GoaParams): F[EitherNec[GoaQueryError, List[GoaSummaryRecord]]] =
    params.instrument.goaName match
      case None           =>
        NonEmptyChain
          .one(GoaQueryError.UnsupportedInstrument(params.instrument.tag))
          .asLeft[List[GoaSummaryRecord]]
          .pure
      case Some(goaInstr) =>
        val uri = buildUri(params, goaInstr)
        executeQuery(uri)

  private def buildUri(params: GoaParams, goaInstr: String): Uri =
    val base = baseUri / "jsonsummary" / "notengineering" / "NotFail" / goaInstr / "OBJECT"

    val withCoords = params match
      case GoaParams.Sidereal(coords, _, searchRadius, _)        =>
        val raDeg    = coords.ra.toAngle.toDoubleDegrees
        val decDeg   = coords.dec.toAngle.toSignedDoubleDegrees
        val srArcsec = searchRadius.toMicroarcseconds.toDouble / 1_000_000.0
        base / s"ra=$raDeg" / s"dec=$decDeg" / s"sr=$srArcsec"
      case GoaParams.NonSidereal(targetName, _, searchRadius, _) =>
        val srArcsec = searchRadius.toMicroarcseconds.toDouble / 1_000_000.0
        base / s"object=$targetName" / s"sr=$srArcsec"

    params.dateRange.fold(withCoords): (start, end) =>
      val startStr = start.format(dateFormatter)
      val endStr   = end.format(dateFormatter)
      withCoords / s"daterange=$startStr-$endStr"

  private def executeQuery(uri: Uri): F[EitherNec[GoaQueryError, List[GoaSummaryRecord]]] =
    val headers             = Headers(
      Header.Raw(ci"User-Agent", GoaClient.UserAgent),
      Header.Raw(ci"Accept-Charset", "UTF-8")
    )
    val request: Request[F] = Request[F](Method.GET, modUri(uri), headers = headers)

    httpClient
      .run(request)
      .use: response =>
        response.status match
          case Status.Ok =>
            response.bodyText.compile.string.map: body =>
              decode[List[GoaSummaryRecord]](body)
                .leftMap(e => NonEmptyChain.one(GoaQueryError.ParseError(e.getMessage)))
          case status    =>
            response.bodyText.compile.string.map: body =>
              NonEmptyChain
                .one(GoaQueryError.RequestError(status.code, body))
                .asLeft[List[GoaSummaryRecord]]
      .handleError: e =>
        NonEmptyChain
          .one(GoaQueryError.NetworkError(e))
          .asLeft[List[GoaSummaryRecord]]
