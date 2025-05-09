// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import org.scalacheck.Cogen

import scala.collection.immutable.SortedMap

trait ArbCollection {
  given  [K: Cogen: Ordering, V: Cogen]: Cogen[SortedMap[K, V]] =
    Cogen[Map[K, V]].contramap(identity)

}

object ArbCollection extends ArbCollection
