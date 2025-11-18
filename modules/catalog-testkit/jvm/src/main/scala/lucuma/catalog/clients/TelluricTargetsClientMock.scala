// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.effect.*
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import fs2.text
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.telluric.TelluricSearchQuery.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.model.TelluricType
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.syntax.literals.*
import org.typelevel.log4cats.Logger

/**
 * Mock TelluricClient for testing purposes.
 */
object TelluricTargetsClientMock:

  given Encoder[TelluricType] =
    Encoder[String].contramap:
      case TelluricType.Hot               => "hot"
      case TelluricType.A0V               => "A0V"
      case TelluricType.Solar             => "Solar"
      case TelluricType.Manual(starTypes) => starTypes.toList.mkString(",")

  given Encoder[TelluricCalibrationOrder] =
    Encoder[String].contramap(_.tag)

  given Encoder[TelluricStar] = star =>
    Json.obj(
      "hip"      -> Json.fromInt(star.hip),
      "spType"   -> star.spType.asJson,
      "ra"       -> Json.fromDoubleOrNull(star.coordinates.ra.toAngle.toDoubleDegrees),
      "dec"      -> Json.fromDoubleOrNull(star.coordinates.dec.toAngle.toSignedDoubleDegrees),
      "distance" -> Json.fromDoubleOrNull(star.distance),
      "hmag"     -> Json.fromDoubleOrNull(star.hmag),
      "score"    -> Json.fromDoubleOrNull(star.score),
      "order"    -> star.order.asJson
    )

  /**
   * Create a mock TelluricClient that returns predefined telluric star responses.
   */
  def mockTelluricClient[F[_]: Concurrent: Logger](
    stars: List[TelluricStar]
  ): F[TelluricTargetsClient[F]] = {
    val responseJson = Json.obj(
      "data" -> Json.obj(
        "search" -> stars.asJson
      )
    )

    val mockHttpClient = Client[F]: _ =>
      Resource.pure:
        Response(Status.Ok)
          .withEntity(responseJson)(using jsonEncoder)

    TelluricTargetsClient.build[F](uri"https://telluric-targets.gpp.gemini.edu/", mockHttpClient)
  }

  /**
   * Create a mock TelluricClient from a JSON string.
   */
  def fromJson[F[_]: Concurrent: Logger](json: Json): F[TelluricTargetsClient[F]] = {
    val mockHttpClient = Client[F]: _ =>
      Resource.pure:
        Response(Status.Ok)
          .withEntity(json)

    TelluricTargetsClient.build[F](uri"https://telluric-targets.gpp.gemini.edu/", mockHttpClient)
  }

  /**
   * Create a mock TelluricClient that reads JSON from a resource file.
   */
  def fromResource[F[_]: Async: Logger](resource: String): F[TelluricTargetsClient[F]] = {
    val jsonStream = readClassLoaderResource[F](resource, 8192).through(text.utf8.decode)

    val mockHttpClient = Client[F]: _ =>
      Resource.eval:
        jsonStream.compile.string.flatMap: content =>
          Response[F](Status.Ok)
            .withEntity(content)
            .pure[F]

    TelluricTargetsClient.build[F](uri"https://telluric-targets.gpp.gemini.edu/", mockHttpClient)
  }

  /**
   * Create a mock TelluricClient that returns an empty list (no telluric stars found).
   */
  def empty[F[_]: Concurrent: Logger]: F[TelluricTargetsClient[F]] =
    mockTelluricClient(List.empty)

  /**
   * Create a mock TelluricClient with a single test star.
   */
  def withSingleStar[F[_]: Concurrent: Logger](star: TelluricStar): F[TelluricTargetsClient[F]] =
    mockTelluricClient(List(star))
