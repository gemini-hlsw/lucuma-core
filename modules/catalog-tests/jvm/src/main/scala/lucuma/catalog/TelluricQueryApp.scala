// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.effect.IOApp
import lucuma.catalog.telluric.TelluricClient
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.TelluricType
import lucuma.core.util.TimeSpan
import org.http4s.Uri
import org.http4s.jdkhttpclient.JdkHttpClient
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object TelluricQueryApp extends IOApp.Simple {

  val telluricUri = Uri.unsafeFromString("https://telluric-targets.gpp.gemini.edu/")

  def run =
    given Logger[IO] = Slf4jLogger.getLogger[IO]

    val coordinates = Declination
      .fromDoubleDegrees(10.0)
      .map(dec => Coordinates(RightAscension.fromDoubleDegrees(150.0), dec))
      .getOrElse(Coordinates.Zero)

    val duration = TimeSpan.fromHours(1.0).getOrElse(TimeSpan.Zero)

    val searchInput = TelluricSearchInput(
      coordinates = coordinates,
      duration = duration,
      brightest = BigDecimal(3.5),
      spType = TelluricType.Hot
    )

    JdkHttpClient
      .simple[IO]
      .use { client =>
        for
          telluricClient <- TelluricClient.create(telluricUri, client)
          results        <- telluricClient.search(searchInput)
          _              <- IO.println(pprint.apply(results))
        yield ()
      }
}
