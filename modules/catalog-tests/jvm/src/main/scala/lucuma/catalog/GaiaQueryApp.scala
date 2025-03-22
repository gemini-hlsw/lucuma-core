// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.syntax.all.*
import fs2.text
import lucuma.catalog.*
import lucuma.core.geom.gmos.all.candidatesArea
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import org.http4s.Method.*
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.jdkhttpclient.JdkHttpClient

trait GaiaQuerySample {
  implicit val gaia: CatalogAdapter.Gaia = CatalogAdapter.Gaia3Lite

  val epoch = Epoch.fromString.getOption("J2022.000").getOrElse(Epoch.J2000)

  implicit val ci: ADQLInterpreter =
    ADQLInterpreter.pmCorrected(1, epoch)

  val m81Coords = (RightAscension.fromStringHMS.getOption("16:17:2.410"),
                   Declination.fromStringSignedDMS.getOption("-22:58:33.90")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  def gaiaQuery[F[_]: Sync](client: Client[F]) = {
    val bc      = BrightnessConstraints(BandsList.GaiaBandsList,
                                   FaintnessConstraint(BrightnessValue.unsafeFrom(16)),
                                   SaturationConstraint(BrightnessValue.unsafeFrom(9)).some
    )
    val query   = CatalogSearch.gaiaSearchUri(QueryByADQL(m81Coords, candidatesArea, bc.some))
    val request = Request[F](GET, query)
    client
      .stream(request)
      .flatMap(
        _.body
          .through(text.utf8.decode)
          .evalTap(a => Sync[F].delay(println(a)))
          .through(CatalogSearch.guideStars[F](gaia))
      )
      .compile
      .toList
  }
}

object GaiaQueryApp extends IOApp.Simple with GaiaQuerySample {
  def run =
    JdkHttpClient
      .simple[IO]
      .flatMap(gaiaQuery[IO])
      .flatMap(x => IO.println(pprint.apply(x)))
}
