// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.IO
import cats.effect.IOApp
import org.http4s.jdkhttpclient.JdkHttpClient

object SimbadQueryApp extends IOApp.Simple with SimbadQuerySample {

  def run =
    JdkHttpClient
      .simple[IO]
      .flatMap(simbadQuery[IO])
      .flatMap(x => IO.println(pprint.apply(x)))
      .void
}
