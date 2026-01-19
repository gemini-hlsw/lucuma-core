// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.Applicative
import cats.data.EitherNec
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import org.http4s.Uri
import org.http4s.client.Client

trait GoaClient[F[_]]:

  def query(params: GoaParams): F[EitherNec[GoaQueryError, List[GoaSummaryRecord]]]

object GoaClient:

  val DefaultBaseUri: Uri = Uri.unsafeFromString("https://archive.gemini.edu")

  val UserAgent: String = "lucuma-core/1.0 (GOA Query Client)"

  inline def build[F[_]: Concurrent](
    httpClient: Client[F],
    baseUri:    Uri = DefaultBaseUri,
    modUri:     Uri => Uri = identity
  ): GoaClient[F] = GoaClientImpl[F](httpClient, baseUri, modUri)

  def noop[F[_]: Applicative]: GoaClient[F] = new GoaClient[F]:
    def query(params: GoaParams): F[EitherNec[GoaQueryError, List[GoaSummaryRecord]]] =
      List.empty[GoaSummaryRecord].rightNec.pure[F]
