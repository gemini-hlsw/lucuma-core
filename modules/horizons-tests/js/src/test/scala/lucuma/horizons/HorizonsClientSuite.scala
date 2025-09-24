// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.effect.IO
import cats.effect.kernel.Resource
import munit.CatsEffectSuite
import org.http4s.dom.FetchClientBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import scala.concurrent.duration.*

trait HorizonsClientSuite extends CatsEffectSuite:

  given Logger[IO] = NoOpLogger.apply

  val client: Resource[IO, HorizonsClient[IO]] = 
    FetchClientBuilder[IO]
      .withRequestTimeout(5.second)
      .resource
      .map(HorizonsClient(_))

