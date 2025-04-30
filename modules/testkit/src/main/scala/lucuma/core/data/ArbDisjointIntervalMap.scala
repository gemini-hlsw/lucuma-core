// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.data.arb

import cats.collections.Diet
import cats.collections.Discrete
import cats.collections.Range
import cats.kernel.Order
import lucuma.core.arb.ArbDiet.given
import lucuma.core.arb.ArbRange.given
import lucuma.core.data.DisjointIntervalMap
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbDisjointIntervalMap:

  def arbDisjointIntervalMap[K: Arbitrary, V: Arbitrary: Discrete: Order]: Arbitrary[DisjointIntervalMap[K, V]] =
    Arbitrary:
      arbitrary[List[(K, Range[V])]].map: es =>
        es.take(5)
          .foldLeft(DisjointIntervalMap.empty[K,V]): 
            case (d, (k, r)) => d.addRange(k, r)

  given[K: Arbitrary, V: Arbitrary: Discrete: Order]: Arbitrary[DisjointIntervalMap[K, V]] =
    arbDisjointIntervalMap

  given[K: Cogen: Ordering, V: Cogen]: Cogen[DisjointIntervalMap[K, V]] =
    Cogen[Map[K, Diet[V]]].contramap(_.toMap)

object ArbDisjointIntervalMap extends ArbDisjointIntervalMap
