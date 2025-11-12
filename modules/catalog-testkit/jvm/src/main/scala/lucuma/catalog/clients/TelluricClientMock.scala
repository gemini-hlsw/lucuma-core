// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.effect.*
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import fs2.text
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.telluric.TelluricClient
import lucuma.catalog.telluric.TelluricStar
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.syntax.literals.*
import org.typelevel.log4cats.Logger

/**
 * Mock TelluricClient for testing purposes.
 */
object TelluricClientMock:

  /**
   * Create a mock TelluricClient that returns predefined telluric star responses.
   */
  def mockTelluricClient[F[_]: Async: Logger](
    stars: List[TelluricStar]
  ): F[TelluricClient[F]] = {
    val responseJson = Json.obj(
      "data" -> Json.obj(
        "search" -> stars.asJson
      )
    )

    val mockHttpClient = Client.fromHttpApp[F](HttpApp[F]: _ =>
      Response[F](Status.Ok)
        .withEntity(responseJson)(using jsonEncoder)
        .pure[F])

    TelluricClient.create[F](uri"http://mock-telluric-service", mockHttpClient)
  }

  /**
   * Create a mock TelluricClient from a JSON string.
   */
  def fromJsonString[F[_]: Async: Logger](jsonString: String): F[TelluricClient[F]] = {
    val mockHttpClient = Client.fromHttpApp[F](HttpApp[F]: _ =>
      Response[F](Status.Ok)
        .withEntity(jsonString)
        .pure[F])

    TelluricClient.create[F](uri"http://mock-telluric-service", mockHttpClient)
  }

  /**
   * Create a mock TelluricClient that reads JSON from a resource file.
   */
  def fromResource[F[_]: Async: Logger](resource: String): F[TelluricClient[F]] = {
    val jsonStream = readClassLoaderResource[F](resource, 8192).through(text.utf8.decode)

    val mockHttpClient = Client.fromHttpApp[F](HttpApp[F]: _ =>
      jsonStream.compile.string.flatMap: content =>
        Response[F](Status.Ok)
          .withEntity(content)
          .pure[F])

    TelluricClient.create[F](uri"http://mock-telluric-service", mockHttpClient)
  }

  /**
   * Create a mock TelluricClient that returns an empty list (no telluric stars found).
   */
  def empty[F[_]: Async: Logger]: F[TelluricClient[F]] =
    mockTelluricClient(List.empty)

  /**
   * Create a mock TelluricClient with a single test star.
   */
  def withSingleStar[F[_]: Async: Logger](star: TelluricStar): F[TelluricClient[F]] =
    mockTelluricClient(List(star))
