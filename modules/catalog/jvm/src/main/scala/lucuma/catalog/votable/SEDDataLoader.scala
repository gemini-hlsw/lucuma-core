// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.IO
import fs2.io.readClassLoaderResource
import fs2.text

object SEDDataLoader:
  def load: IO[SEDDataConfig] =
    for
      starsContent   <- loadResource("match_sed_stars.dat")
      gravityContent <- loadResource("match_sed_log_g.csv")
      config         <- IO.fromEither(
                          (for
                            stars   <- SEDDataParsers.parseStarsFile(starsContent)
                            gravity <- SEDDataParsers.parseGravityFile(gravityContent)
                          yield SEDDataConfig(stars, gravity)).left.map(new RuntimeException(_))
                        )
    yield config

  private def loadResource(name: String): IO[String] =
    readClassLoaderResource[IO](s"lucuma/catalog/$name")
      .through(text.utf8.decode)
      .compile
      .string
