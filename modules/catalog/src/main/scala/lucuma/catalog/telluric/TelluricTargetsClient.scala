// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all.*
import clue.http4s.Http4sHttpBackend
import clue.http4s.Http4sHttpClient
import clue.FetchClient
import clue.syntax.*
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import lucuma.catalog.telluric.TelluricSearchQuery.TelluricStar
import lucuma.core.model.TelluricType

/**
 * Client for the telluric targets service
 */
trait TelluricTargetsClient[F[_]]:
  def search(input: TelluricSearchInput): F[List[TelluricStar]]

object TelluricTargetsClient:
  def build[F[_]: Concurrent: Logger](
    uri:    Uri,
    client: Client[F]
  ): F[TelluricTargetsClient[F]] =
    given Http4sHttpBackend[F] = Http4sHttpBackend[F](client)

    Http4sHttpClient
      .of[F, TelluricService](uri)
      .map: http =>
        given FetchClient[F, TelluricService] = http

        new TelluricTargetsClient[F]:
          override def search(input: TelluricSearchInput): F[List[TelluricStar]] =
            TelluricSearchQuery[F]
              .query(
                input.coordinates.ra.toAngle.toDoubleDegrees.toFloat,
                input.coordinates.dec.toAngle.toSignedDoubleDegrees.toFloat,
                input.duration.toHours.toDouble.toFloat,
                input.brightest.toFloat,
                input.spType match
                  case TelluricType.Hot               => "hot"
                  case TelluricType.A0V               => "A0V"
                  case TelluricType.Solar             => "Solar"
                  case TelluricType.Manual(starTypes) =>
                    starTypes.toList.mkString(",")
              )
              .raiseGraphQLErrors
              .map(_.search)

  def noop[F[_]: Applicative]: TelluricTargetsClient[F] = new TelluricTargetsClient[F]:
    def search(input: TelluricSearchInput): F[List[TelluricStar]] =
      List.empty.pure[F]
