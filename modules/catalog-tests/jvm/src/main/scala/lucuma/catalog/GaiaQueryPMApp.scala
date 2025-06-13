// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyList
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
import lucuma.core.math.ProperMotion
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import org.http4s.jdkhttpclient.JdkHttpClient

trait GaiaQueryPMSample {
  val epoch = Epoch.fromString.getOption("J2022.000").getOrElse(Epoch.J2000)

  given ADQLInterpreter = ADQLInterpreter.nTarget(100)

  val m81Coords = (RightAscension.fromStringHMS.getOption("16:17:2.410"),
                   Declination.fromStringSignedDMS.getOption("-22:58:33.90")
  ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

  val start = Coordinates.fromHmsDms.getOption("16:17:15.580490 -23:02:42.794440").get
  val end   = Coordinates.fromHmsDms.getOption("16:16:49.269782 -22:54:25.255776").get

  val pm = ProperMotion(ProperMotion.μasyRA(-6060), ProperMotion.μasyDec(8298))

  val tracking = SiderealTracking(m81Coords, Epoch.J2000, pm.some, none, none)

  def gaiaQuery[F[_]: Sync](client: GaiaClient[F]) = {
    val bc               = BrightnessConstraints(
      BandsList.GaiaBandsList,
      FaintnessConstraint(BrightnessValue.unsafeFrom(16)),
      SaturationConstraint(BrightnessValue.unsafeFrom(9)).some
    )
    val query: ADQLQuery =
      CoordinatesRangeQueryByADQL(
        NonEmptyList.of(start, end),
        candidatesArea,
        bc.some
      )
    //   TimeRangeQueryByADQL(
    //     tracking,
    //     Interval
    //       .closed(Instant.EPOCH, Instant.EPOCH.plusSeconds(365 * 24 * 60 * 60 * 60))
    //       .asInstanceOf[Bounded[Instant]],
    //     candidatesArea,
    //     bc.some
    //   )

    client.query(query).map(r => println(pprint(r)))
  }
}

object GaiaQueryPMApp extends IOApp.Simple with GaiaQueryPMSample {
  def run =
    JdkHttpClient
      .simple[IO]
      .map(GaiaClient.build(_))
      .use(gaiaQuery[IO])
      .flatMap(x => IO.println(pprint.apply(x)))
}
