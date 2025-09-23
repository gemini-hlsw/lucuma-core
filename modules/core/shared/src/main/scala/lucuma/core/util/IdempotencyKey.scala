// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import monocle.Prism

import java.util.UUID
import scala.util.Try

object IdempotencyKey extends NewType[UUID]:

  val FromString: Prism[String, IdempotencyKey] =
    def parse(s: String): Option[IdempotencyKey] =
      Try(UUID.fromString(s)).toOption.map(IdempotencyKey.apply)

    def show(k: IdempotencyKey): String =
      k.value.toString.toLowerCase

    Prism(parse)(show)

type IdempotencyKey = IdempotencyKey.Type