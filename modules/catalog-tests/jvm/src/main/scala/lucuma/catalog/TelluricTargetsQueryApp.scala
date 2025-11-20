// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.effect.IOApp
import lucuma.catalog.clients.SimbadClient
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.TelluricType
import lucuma.core.util.TimeSpan
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.syntax.literals.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object TelluricTargetsQueryApp extends IOApp.Simple:

  val telluricUri = uri"https://telluric-targets.gpp.gemini.edu/"

  def run =
    given Logger[IO] = Slf4jLogger.getLogger[IO]

    val coordinates =
      Coordinates(
        RightAscension.fromDoubleDegrees(150.0),
        Declination.fromDoubleDegrees(10.0).get
      )

    val duration = TimeSpan.fromHours(1.0).get

    val searchInput = TelluricSearchInput(
      coordinates = coordinates,
      duration = duration,
      brightest = BigDecimal(3.5),
      spType = TelluricType.Hot
    )

    JdkHttpClient
      .simple[IO]
      .use: client =>
        val simbadClient = SimbadClient.build(client)
        for
          telluricClient <- TelluricTargetsClient.build(telluricUri, client, simbadClient)
          results        <- telluricClient.searchTarget(searchInput)
          _              <- IO.println(pprint.apply(results))
        yield ()
