// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.io.net.tls.TLSContext
import munit.CatsEffectSuite
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

trait HorizonsClientSuite extends CatsEffectSuite:

  given Logger[IO] = Slf4jLogger.getLoggerFromName("HorizonsClientSuite")

  val client: Resource[IO, HorizonsClient[IO]] = 
    for 
      tctx <- TLSContext.Builder.forAsync[IO].insecureResource
      http <- EmberClientBuilder.default[IO].withTLSContext(tctx).withTimeout(5.seconds).build
    yield HorizonsClient(http)

