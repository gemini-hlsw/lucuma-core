// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.IO
import org.scalajs.dom.Fetch
import org.scalajs.dom.HttpMethod
import org.scalajs.dom.RequestInit

import scala.scalajs.js

object SEDDataLoader:
  def load(baseUrl: String): IO[SEDDataConfig] =
    for
      starsContent   <- fetchFile(s"$baseUrl/match_sed_stars.dat")
      gravityContent <- fetchFile(s"$baseUrl/match_sed_log_g.csv")
      config         <- IO.fromEither(
                          (for
                            stars   <- SEDDataParsers.parseStarsFile(starsContent)
                            gravity <- SEDDataParsers.parseGravityFile(gravityContent)
                          yield SEDDataConfig(stars, gravity)).left.map(new RuntimeException(_))
                        )
    yield config

  private def fetchFile(url: String): IO[String] =
    IO.fromPromise(IO {
      val init = new RequestInit {}
      init.method = HttpMethod.GET
      Fetch
        .fetch(url, init)
        .`then`[String](response =>
          if response.ok then response.text()
          else js.Promise.reject(js.Error(s"Failed to fetch $url: ${response.status}"))
        )
    })
