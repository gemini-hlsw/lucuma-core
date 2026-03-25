// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.ags.*
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.geom.pwfs.patrolField
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

object GhostAgsSelectionApp extends IOApp.Simple with AgsSelectionSample {

  given LoggerFactory[IO] = NoOpFactory[IO]

  val ghostParams = AgsParams.GhostIfu()

  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build[IO](_))
      .use(gaiaClient =>
        gaiaClient
          .queryGuideStars:
            QueryByADQL(tracking.at(now).get,
                        patrolField.patrolField,
                        widestConstraints.some,
                        DefaultAreaBuffer
            )
          .map:
            _.collect { case Right(t) => t }
              .map(GuideStarCandidate.siderealTarget.get)
          .map { candidates =>
            println(s"GHOST candidates: ${candidates.length}")
            val r = Ags
              .agsAnalysis(
                constraints,
                wavelength,
                coords,
                List(coords),
                None,
                posAngles,
                Some(acqOffsets),
                Some(sciOffsets),
                ghostParams,
                candidates
              )
            pprint.pprintln(r.sortUsablePositions)
            r.sortUsablePositions
          }
      )
      .flatTap(x => IO.println(x.length))
      .void
}
