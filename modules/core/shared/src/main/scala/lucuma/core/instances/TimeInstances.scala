// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.instances

import cats.kernel.Order

import java.time.Instant

trait TimeInstances:
  given Order[Instant] = Order.by(_.toEpochMilli)

object time extends TimeInstances