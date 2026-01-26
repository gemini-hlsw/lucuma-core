// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.IO
import cats.effect.IOApp
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import org.http4s.jdkhttpclient.JdkHttpClient

object SimbadQueryApp extends IOApp.Simple with SimbadQuerySample {

  def run =
    JdkHttpClient.simple[IO].use { client =>
      for
        sedConfig <- SEDDataLoader.load
        result    <- simbadQuery[IO](client, SEDMatcher.fromConfig(sedConfig))
        _         <- IO.println(pprint.apply(result))
      yield ()
    }
}
