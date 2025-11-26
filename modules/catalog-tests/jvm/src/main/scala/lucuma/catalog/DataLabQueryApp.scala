// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.syntax.all.*
import lucuma.catalog.*
import lucuma.catalog.clients.GaiaClient
import lucuma.core.geom.gmos.all.candidatesArea
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import org.http4s.jdkhttpclient.JdkHttpClient

/**
 * Test app for querying Gaia data via NOIRLab DataLab.
 *
 * Usage:
 *   sbt "catalogTestsJVM/runMain lucuma.catalog.votable.DataLabQueryApp"
 *   sbt "catalogTestsJVM/runMain lucuma.catalog.votable.DataLabQueryApp <auth-token>"
 *
 * If no token is provided, uses anonymous access.
 */
object DataLabQueryApp extends IOApp.Simple {
  val epoch = Epoch.fromString.getOption("J2016.000").getOrElse(Epoch.J2000)

  given ADQLInterpreter = ADQLInterpreter.oneTarget

  val testCoords = (RightAscension.fromStringHMS.getOption("16:17:2.410"),
                    Declination.fromStringSignedDMS.getOption("-22:58:33.90")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  def dataLabQuery[F[_]: Sync](client: GaiaClient[F]) = {
    val bc = BrightnessConstraints(
      BandsList.GaiaBandsList,
      FaintnessConstraint(BrightnessValue.unsafeFrom(16)),
      SaturationConstraint(BrightnessValue.unsafeFrom(9)).some
    )

    client.query(QueryByADQL(testCoords, candidatesArea, bc.some)).map(r => println(pprint(r)))
  }

  def dataLabQueryById[F[_]: Sync](client: GaiaClient[F], sourceId: Long) =
    client.queryById(sourceId).map(r => println(pprint(r)))

  def run = {
    val args = scala.sys.props.get("app.args").map(_.split(" ").toList).getOrElse(Nil)

    val adapter = args.headOption match {
      case Some(token) =>
        IO.println(s"Using custom auth token") *>
          IO.pure(CatalogAdapter.gaia3DataLabWithToken(token))
      case None        =>
        IO.println("Using anonymous access") *>
          IO.pure(CatalogAdapter.Gaia3DataLab)
    }

    adapter.flatMap { a =>
      JdkHttpClient
        .simple[IO]
        .map(client => GaiaClient.build(client, adapters = NonEmptyChain.one(a)))
        .use { client =>
          for {
            _ <- IO.println(s"Querying DataLab at ${a.uri}")
            _ <- IO.println(s"Test coordinates: $testCoords")
            _ <- IO.println("--- Cone search ---")
            _ <- dataLabQuery[IO](client)
            _ <- IO.println("\n--- Query by ID (538670232718296576) ---")
            _ <- dataLabQueryById[IO](client, 538670232718296576L)
          } yield ()
        }
    }
  }
}
