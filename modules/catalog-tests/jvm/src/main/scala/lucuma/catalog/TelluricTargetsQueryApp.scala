// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.catalog.clients.SimbadClient
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import lucuma.catalog.telluric.TelluricSearchInput
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.TelluricType
import lucuma.core.util.TimeSpan
import org.http4s.jdkhttpclient.JdkHttpClient
import org.http4s.syntax.literals.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

object TelluricTargetsQueryApp extends IOApp.Simple:

  val telluricUri = uri"https://telluric-targets.gpp.gemini.edu/"

  // Manual uses temperature classes.
  val searchTypes: List[TelluricType] = List(
    TelluricType.Hot,
    TelluricType.Solar,
    TelluricType.A0V,
    TelluricType.Manual(NonEmptyList.of("A0", "A2"))
  )

  def run =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val coordinates =
      Coordinates(
        RightAscension.fromDoubleDegrees(150.0),
        Declination.fromDoubleDegrees(10.0).get
      )

    val duration = TimeSpan.fromHours(1.0).get

    JdkHttpClient
      .simple[IO]
      .use: client =>
        for
          sedConfig      <- SEDDataLoader.load[IO]
          simbadClient    = SimbadClient.build(client, SEDMatcher.fromConfig(sedConfig))
          telluricClient <- TelluricTargetsClient.build(telluricUri, client, simbadClient)
          _              <- searchTypes.traverse_ : spType =>
                              val searchInput = TelluricSearchInput(
                                coordinates = coordinates,
                                duration = duration,
                                brightest = BigDecimal(3.5),
                                spType = spType
                              )
                              telluricClient
                                .searchTarget(searchInput)
                                .attempt
                                .flatMap:
                                  case Right(results) =>
                                    IO.println(s"=== $spType: ${results.size} star(s)") *>
                                      IO.println(pprint.apply(results))
                                  case Left(err)      =>
                                    IO.println(s"=== $spType FAILED: ${err.getMessage}")
        yield ()
