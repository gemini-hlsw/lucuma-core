// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.*
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import org.http4s.dom.FetchClientBuilder

import scala.scalajs.js.annotation.*

@JSExportTopLevel("main")
object SimbadQueryApp extends IOApp.Simple with SimbadQuerySample {

  val sedDataBaseUrl = "/lucuma/catalog"

  def run =
    FetchClientBuilder[IO].resource.use { client =>
      for
        sedConfig <- SEDDataLoader.load(sedDataBaseUrl)
        result    <- simbadQuery[IO](client, SEDMatcher.fromConfig(sedConfig))
        _         <- IO.println(pprint.apply(result))
      yield ()
    }

}
