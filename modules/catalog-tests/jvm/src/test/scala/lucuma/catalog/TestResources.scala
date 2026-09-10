// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import fs2.Stream
import fs2.io.readClassLoaderResource

/**
 * sbt 2 serves test resources out of a jar, so a resource URL is not a filesystem path and cannot
 * be handed to Files. Everything here goes through the classloader instead.
 */
object TestResources:

  def bytes(name: String): Stream[IO, Byte] =
    readClassLoaderResource[IO](name.stripPrefix("/"))

  def allBytes(name: String): Array[Byte] =
    val in = getClass.getResourceAsStream(name)
    try in.readAllBytes()
    finally in.close()
