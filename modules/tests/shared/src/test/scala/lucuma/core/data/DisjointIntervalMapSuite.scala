// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data

import cats.collections.Diet
import cats.collections.Range
import cats.kernel.laws.discipline.CommutativeSemigroupTests
import cats.kernel.laws.discipline.EqTests
import cats.syntax.all.*
import lucuma.core.arb.ArbRange.given
import lucuma.core.data.arb.ArbDisjointIntervalMap.given
import org.scalacheck.Prop.*

// N.B. using Byte a lot here to encourage collisions
final class DisjointIntervalMapSuite extends munit.DisciplineSuite:

  checkAll("CommutativeSemigroup[DisjointIntervalMap]", CommutativeSemigroupTests[DisjointIntervalMap[Byte, Byte]].semigroup)
  checkAll("Eq[DisjointIntervalMap]", EqTests[DisjointIntervalMap[Byte, Byte]].eqv)

  test("toMap <-> unsafeFromMap"):
    forAll: (d1: DisjointIntervalMap[Byte, Byte]) =>
      val d2 = DisjointIntervalMap.unsafeFromMap(d1.toMap)
      assert(d1 === d2)

  test("intersect"):
    forAll: (ra: Range[Byte], rb: Range[Byte]) =>
      val da = DisjointIntervalMap.one("a", ra)
      val db = DisjointIntervalMap.one("a", rb)
      val dc = da.intersect(db)
      val expected: DisjointIntervalMap[String, Byte] =
        if ra.overlaps(rb) then
          DisjointIntervalMap.one(
            "a",
            (Diet.fromRange(ra) & Diet.fromRange(rb)).toIterator.next
          )
        else
          DisjointIntervalMap.empty[String, Byte]
      assert(dc === expected)

  test("add"):
    forAll: (d: DisjointIntervalMap[Byte, Byte], k: Byte, v: Byte) =>
      val dʹ = d.add(k, v)      
      assert(dʹ.getKeyForValue(v) === Some(k))
      assert(dʹ.get(k).map(_.contains(v)) === Some(true))      

  test("addRange"):
    forAll: (d: DisjointIntervalMap[Byte, Byte], k: Byte, v: Range[Byte]) =>
      val dʹ = d.addRange(k, v)      
      assert(dʹ.getKeyForRange(v) === Some(k))
      assert(dʹ.get(k).map(_.containsRange(v)) === Some(true))

  test("add <-> remove"):
    forAll: (d: DisjointIntervalMap[Byte, Byte], k: Byte, v: Byte) =>
      d.getKeyForValue(v) match
        case None     => assert(d.add(k, v).remove(k, v) === d)
        case Some(kʹ) => assert(d.remove(kʹ, v).add(kʹ, v) === d)
      
  test("addRange <-> removeRange"):
    forAll: (d: DisjointIntervalMap[Byte, Byte], k: Byte, v: Byte) =>
      d.getKeyForValue(v) match
        case None     => assert(d.add(k, v).remove(k, v) === d)
        case Some(kʹ) => assert(d.remove(kʹ, v).add(kʹ, v) === d)
