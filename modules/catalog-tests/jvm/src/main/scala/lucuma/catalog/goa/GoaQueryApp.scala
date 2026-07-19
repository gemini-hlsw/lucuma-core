// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import org.http4s.jdkhttpclient.JdkHttpClient

object GoaQueryApp extends IOApp.Simple:

  val ngc4150Coords: Coordinates = (
    RightAscension.fromStringHMS.getOption("12:10:33.6"),
    Declination.fromStringSignedDMS.getOption("+30:24:06")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  val searchRadius: Angle = Angle.fromDoubleArcseconds(120.0)

  def run: IO[Unit] =
    JdkHttpClient
      .simple[IO]
      .map(GoaClient.build[IO](_))
      .use: client =>
        val params = GoaParams.Sidereal(ngc4150Coords, Instrument.GmosNorth, searchRadius)

        IO.println(s"Querying GOA for ${params.instrument} at ${ngc4150Coords}...") *>
          client
            .query(params)
            .flatMap:
              case Right(records) =>
                IO.println(s"Found ${records.length} records:") *>
                  records.traverse_ : record =>
                    IO.println(
                      s"  ${record.name} | ${record.dataLabel.getOrElse("N/A")} | ${record.objectName.getOrElse("N/A")} | ${record.qaState.getOrElse("N/A")}"
                    )
              case Left(errors)   =>
                IO.println(s"Query failed:") *>
                  errors.toList.traverse_(e => IO.println(s"  - ${e.message}"))
