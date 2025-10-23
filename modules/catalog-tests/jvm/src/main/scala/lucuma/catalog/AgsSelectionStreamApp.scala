// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import fs2.*
import lucuma.ags.*
import lucuma.catalog.clients.GaiaClient
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.http4s.jdkhttpclient.JdkHttpClient

object AgsSelectionSampleStreamApp extends IOApp.Simple with AgsSelectionSample:

  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build(_))
      .use(
        gaiaQuery[IO](_)
          .flatMap(candidates =>
            fs2.Stream
              .emits[IO, GuideStarCandidate](candidates)
              .through(
                Ags.agsAnalysisStream[IO](
                  constraints,
                  wavelength,
                  coords,
                  List(coords),
                  NonEmptyList.of(
                    Angle.fromDoubleDegrees(-120),
                    Angle.fromDoubleDegrees(120)
                  ),
                  NonEmptyList.of(Offset.Zero).some,
                  NonEmptyList.of(Offset.Zero).some,
                  AgsParams.GmosAgsParams(
                    GmosNorthFpu.LongSlit_1_00.asLeft.some,
                    PortDisposition.Side
                  ),
                  None
                )
              )
              .compile
              .toList
          )
      )
      .flatTap(x => IO.println(x.length))
      // .flatMap(x => IO.println(x.head))
      .flatMap(x => x.filter(_.isUsable).traverse(u => IO(pprint.pprintln(u))))
      .void
