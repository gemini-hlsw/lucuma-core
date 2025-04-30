// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.instances

import cats.collections.Discrete
import cats.kernel.LowerBounded
import cats.kernel.UpperBounded

import java.time.Instant

trait InstantInstances extends org.typelevel.cats.time.instances.instant:

  given Discrete[Instant] with LowerBounded[Instant] with UpperBounded[Instant] with
    def pred(x: Instant) = Instant.ofEpochMilli(x.toEpochMilli - 1)
    def succ(x: Instant) = Instant.ofEpochMilli(x.toEpochMilli + 1)
    def minBound = Instant.MIN
    def maxBound = Instant.MAX
    def partialOrder = summon

object instant extends InstantInstances