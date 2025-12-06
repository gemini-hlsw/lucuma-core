// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import lucuma.catalog.*
import lucuma.catalog.clients.GaiaClient
import lucuma.core.geom.gmos.all.candidatesArea
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import org.http4s.jdkhttpclient.JdkHttpClient

object DataLabQueryApp extends IOApp.Simple:

  given ADQLInterpreter = ADQLInterpreter.nTarget(5)

  val testCoords = (RightAscension.fromStringHMS.getOption("16:17:2.410"),
                    Declination.fromStringSignedDMS.getOption("-22:58:33.90")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  val testSourceId = 538670232718296576L

  def run =
    JdkHttpClient
      .simple[IO]
      .use: httpClient =>
        val dataLabClient =
          GaiaClient.build(httpClient, adapters = NonEmptyChain.one(CatalogAdapter.Gaia3DataLab))

        coneSearch(dataLabClient) *>
          runQueryById(dataLabClient, testSourceId)

  private def coneSearch(client: GaiaClient[IO]) = {
    val bc = BrightnessConstraints(
      BandsList.GaiaBandsList,
      FaintnessConstraint(BrightnessValue.unsafeFrom(16)),
      SaturationConstraint(BrightnessValue.unsafeFrom(9)).some
    )

    client
      .query(QueryByADQL(testCoords, candidatesArea, bc.some))
      .flatMap(r => IO.println(pprint(r)))
  }

  private def runQueryById(client: GaiaClient[IO], sourceId: Long) =
    client.queryById(sourceId).flatMap(r => IO.println(pprint(r)))
