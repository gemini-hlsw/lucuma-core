// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import lucuma.core.optics.Format

import java.util.UUID
import scala.util.Try

object IdempotencyKey extends NewType[UUID]:

  def fromStringUnsafe(s: String): IdempotencyKey =
    IdempotencyKey(UUID.fromString(s))

  def fromString(s: String): Option[IdempotencyKey] =
    Try(UUID.fromString(s)).toOption.map(IdempotencyKey.apply)

  val FromString: Format[String, IdempotencyKey] =
    Format(fromString, _.value.toString.toLowerCase)

type IdempotencyKey = IdempotencyKey.Type