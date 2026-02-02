// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.effect.IO
import cats.syntax.either.*
import fs2.io.readClassLoaderResource
import fs2.text

object SEDDataLoader:
  def load: IO[SEDDataConfig] =
    for {
      starsContent   <- loadResource("match_sed_stars.dat")
      gravityContent <- loadResource("match_sed_log_g.csv")
      stars          <- SEDDataParsers.parseStarsFile(starsContent).liftTo[IO]
      gravity        <- SEDDataParsers.parseGravityFile(gravityContent).liftTo[IO]
    } yield SEDDataConfig(stars, gravity)

  def loadMatcher: IO[SEDMatcher] =
    load.map(SEDMatcher.fromConfig)

  private def loadResource(name: String): IO[String] =
    readClassLoaderResource[IO](s"lucuma/catalog/$name")
      .through(text.utf8.decode)
      .compile
      .string
