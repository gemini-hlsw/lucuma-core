// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.clients

import cats.Applicative
import cats.data.EitherNec
import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.io.readClassLoaderResource
import fs2.text
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.votable.CatalogProblem
import lucuma.core.model.Target
import org.http4s.*
import org.http4s.client.Client

import scala.xml.Node
import scala.xml.Utility

object SimbadClientMock:

  /**
   * Create a mock SimbadClient that returns VOTable XML responses.
   */
  def mockSimbadClient[F[_]: Concurrent](
    voTableXml: Stream[F, String]
  ): SimbadClient[F] =
    val mockHttpClient = Client[F]: _ =>
      Resource.pure(Response(Status.Ok).withEntity(voTableXml))

    SimbadClient.build[F](mockHttpClient)

  /**
   * Mock SimbadClient that reads XML.
   */
  def fromXML[F[_]: Concurrent](xml: Node): SimbadClient[F] =
    mockSimbadClient(Stream.emit(Utility.trim(xml).toString))

  /**
   * Mock SimbadClient that reads a String.
   */
  def fromString[F[_]: Concurrent](content: String): SimbadClient[F] =
    mockSimbadClient(Stream.emit(content))

  /**
   * Mock SimbadClient that reads VOTable XML from a resource file.
   */
  def fromResource[F[_]: Async](resource: String): SimbadClient[F] =
    mockSimbadClient(readClassLoaderResource[F](resource, 8192).through(text.utf8.decode))

  /**
   * Mock SimbadClient with empty results.
   */
  def empty[F[_]: Applicative]: SimbadClient[F] = SimbadClient.noop

  /**
   * Mock SimbadClient that returns a single result.
   */
  def withSingleResult[F[_]: Applicative](result: CatalogTargetResult): SimbadClient[F] =
    new SimbadClient[F]:
      def search(name: NonEmptyString): F[EitherNec[CatalogProblem, CatalogTargetResult]] =
        result.rightNec.pure[F]

      def search(
        term:       NonEmptyString,
        wildcard:   Boolean,
        maxResults: Option[Int]
      ): F[List[CatalogTargetResult]] =
        List(result).pure[F]

  def withSingleTarget[F[_]: Applicative](target: Target.Sidereal): SimbadClient[F] =
    withSingleResult(CatalogTargetResult(target, None))
