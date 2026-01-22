// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import fs2.*
import lucuma.ags.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.ScienceOffsets
import lucuma.ags.syntax.*
import lucuma.catalog.clients.GaiaClient
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

object AgsSelectionSampleStreamApp extends IOApp.Simple with AgsSelectionSample:

  given LoggerFactory[IO] = NoOpFactory[IO]

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
                  None,
                  NonEmptyList.of(
                    Angle.fromDoubleDegrees(-120),
                    Angle.fromDoubleDegrees(120)
                  ),
                  Some(AcquisitionOffsets(NonEmptySet.of(Offset.Zero.guided))),
                  Some(ScienceOffsets(NonEmptySet.of(Offset.Zero.guided))),
                  AgsParams.GmosLongSlit(
                    GmosNorthFpu.LongSlit_1_00.asLeft,
                    PortDisposition.Side
                  )
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
