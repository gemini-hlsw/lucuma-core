// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.effect.Sync
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import fs2.text

object SEDDataLoader:
  def load[F[_]: Sync]: F[SEDDataConfig] =
    for {
      starsContent   <- loadResource[F]("match_sed_stars.dat")
      gravityContent <- loadResource[F]("match_sed_log_g.csv")
      stars          <- SEDDataParsers.parseStarsFile(starsContent).liftTo[F]
      gravity        <- SEDDataParsers.parseGravityFile(gravityContent).liftTo[F]
    } yield SEDDataConfig(stars, gravity)

  def loadMatcher[F[_]: Sync]: F[SEDMatcher] =
    load[F].map(SEDMatcher.fromConfig)

  private def loadResource[F[_]: Sync](name: String): F[String] =
    readClassLoaderResource[F](s"lucuma/catalog/$name")
      .through(text.utf8.decode)
      .compile
      .string
