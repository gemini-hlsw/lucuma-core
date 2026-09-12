// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import fs2.Stream
import fs2.io.readClassLoaderResource

/**
 * Reads a classpath resource, for tests that run against a fixture file.
 *
 * Resources reach the classpath inside a jar under sbt 2, so a resource URL is not a filesystem
 * path and cannot be handed to `Files`. Going through the classloader works either way.
 */
object TestResources:

  /** The resource as a byte stream. `name` is absolute, with or without the leading slash. */
  def stream(name: String): Stream[IO, Byte] =
    readClassLoaderResource[IO](name.stripPrefix("/"))

  /** The whole resource, for fixtures small enough to hold in memory. */
  def bytes(name: String): Array[Byte] =
    val in = getClass.getResourceAsStream(name)
    try in.readAllBytes()
    finally in.close()
