// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

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

trait GaiaQuerySample {
  val epoch = Epoch.fromString.getOption("J2016.000").getOrElse(Epoch.J2000)

  given ADQLInterpreter = ADQLInterpreter.oneTarget

  val m81Coords = (RightAscension.fromStringHMS.getOption("16:17:2.410"),
                   Declination.fromStringSignedDMS.getOption("-22:58:33.90")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  def gaiaQuery[F[_]: Sync](client: GaiaClient[F]) = {
    val bc = BrightnessConstraints(
      BandsList.GaiaBandsList,
      FaintnessConstraint(BrightnessValue.unsafeFrom(16)),
      SaturationConstraint(BrightnessValue.unsafeFrom(9)).some
    )

    client.query(QueryByADQL(m81Coords, candidatesArea, bc.some)).map(r => println(pprint(r)))
  }
}

object GaiaQueryApp extends IOApp.Simple with GaiaQuerySample {
  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build(_))
      .use(gaiaQuery[IO])
      .flatMap(x => IO.println(pprint.apply(x)))
}
