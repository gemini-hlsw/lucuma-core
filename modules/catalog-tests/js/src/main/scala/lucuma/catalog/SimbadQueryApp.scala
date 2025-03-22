// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.*
import org.http4s.dom.FetchClientBuilder

import scala.scalajs.js.annotation.*

@JSExportTopLevel("main")
object SimbadQueryApp extends IOApp.Simple with SimbadQuerySample {

  def run =
    FetchClientBuilder[IO].resource
      .use(simbadQuery[IO])
      .flatMap(x => IO.println(pprint.apply(x)))
      .void

}
