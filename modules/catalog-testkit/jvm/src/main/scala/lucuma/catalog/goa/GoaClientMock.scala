// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import fs2.text
import io.circe.parser.decode

object GoaClientMock:

  import codecs.given

  def fromJson[F[_]: Concurrent](json: String): GoaClient[F] =
    new GoaClient[F]:
      def query(params: GoaParams): F[EitherNec[GoaQueryError, List[GoaSummaryRecord]]] =
        GoaInstrument.toGoaName(params.instrument) match
          case None    =>
            NonEmptyChain
              .one(GoaQueryError.UnsupportedInstrument(params.instrument.tag))
              .asLeft[List[GoaSummaryRecord]]
              .pure[F]
          case Some(_) =>
            decode[List[GoaSummaryRecord]](json)
              .leftMap(e => NonEmptyChain.one(GoaQueryError.ParseError(e.getMessage)))
              .pure[F]

  def fromResource[F[_]: Async](resource: String): F[GoaClient[F]] =
    readClassLoaderResource[F](resource, 8192)
      .through(text.utf8.decode)
      .compile
      .string
      .map(fromJson[F])

  def empty[F[_]: Concurrent]: GoaClient[F] =
    fromJson[F]("[]")
