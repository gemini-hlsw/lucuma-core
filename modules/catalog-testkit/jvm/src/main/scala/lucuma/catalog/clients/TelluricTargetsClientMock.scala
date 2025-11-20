// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.effect.*
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import fs2.text
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.telluric.TelluricStar
import lucuma.catalog.telluric.TelluricTargetsClient
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.syntax.literals.*
import org.typelevel.log4cats.Logger

/**
 * Mock TelluricClient for testing purposes.
 */
object TelluricTargetsClientMock:

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
