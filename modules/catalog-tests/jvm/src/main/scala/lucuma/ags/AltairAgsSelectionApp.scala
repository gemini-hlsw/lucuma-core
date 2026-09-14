// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Wavelength
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

/**
 * GNIRS long slit behind Altair. The catalog query is the AOWFS candidates circle with the
 * slowest LGS limits in R (the widest Altair constraint), then the same candidates are analysed
 * in NGS and LGS mode.
 */
object AltairAgsSelectionApp extends IOApp.Simple with AgsSelectionSample {
  given LoggerFactory[IO] = NoOpFactory[IO]

  val altairWavelength: Wavelength = Wavelength.fromIntNanometers(1650).get

  val gnirsParams: AgsParams.GnirsLongSlit =
    AgsParams.GnirsLongSlit(
      GnirsFpuSlit.LongSlit_0_30,
      GnirsCamera.LongBlue,
      GnirsPrism.Mirror,
      PortDisposition.Bottom
    )

  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build[IO](_))
      .use(gaiaClient =>
        gaiaClient
          .queryGuideStars:
            QueryByADQL(
              tracking.at(now).get,
              GuideProbe.AltairAOWFS.candidatesArea,
              guideStarBrightnessConstraints(
                constraints,
                GuideProbe.AltairAOWFS,
                AltairMode.Lgs.some,
                GuideSpeed.Slow,
                altairWavelength
              ).some,
              DefaultAreaBuffer
            )
          .map:
            _.collect { case Right(t) => t }
              .map(GuideStarCandidate.siderealTarget.get)
          .map { candidates =>
            println(s"Altair candidates: ${candidates.length}")
            candidates.foreach(candidate => println(s"  ${candidate.name} ${candidate.brightnesses}"))
            List(AltairMode.Ngs, AltairMode.Lgs).map { mode =>
              val result = Ags
                .agsAnalysis(
                  constraints,
                  altairWavelength,
                  coords,
                  List(coords),
                  None,
                  posAngles,
                  Some(acqOffsets),
                  Some(sciOffsets),
                  gnirsParams.withAltair(mode),
                  candidates
                )
              println(s"$mode: ${result.stats.show}")
              pprint.pprintln(result.analyses.sortUsablePositions)
              result.analyses.sortUsablePositions
            }
          }
      )
      .flatTap(usable => IO.println(usable.map(_.length)))
      .void
}
