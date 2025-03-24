// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import fs2.*
import lucuma.ags.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import org.http4s.jdkhttpclient.JdkHttpClient

object AgsSelectionSampleStreamApp extends IOApp.Simple with AgsSelectionSample:

  def run =
    JdkHttpClient
      .simple[IO]
      .flatMap(
        gaiaQuery[IO](_)
          .map(GuideStarCandidate.siderealTarget.get)
          .through(
            Ags
              .agsAnalysisStream(
                constraints,
                wavelength,
                coords,
                List(coords),
                NonEmptyList.of(AgsPosition(Angle.fromDoubleDegrees(-120), Offset.Zero),
                                AgsPosition(Angle.fromDoubleDegrees(120), Offset.Zero)
                ),
                AgsParams.GmosAgsParams(
                  GmosNorthFpu.LongSlit_1_00.asLeft.some,
                  PortDisposition.Side
                )
              )
          )
          .compile
          .toList
      )
      .flatTap(x => IO.println(x.length))
      // .flatMap(x => IO.println(x.head))
      .flatMap(x => x.filter(_.isUsable).traverse(u => IO(pprint.pprintln(u))))
      .void
