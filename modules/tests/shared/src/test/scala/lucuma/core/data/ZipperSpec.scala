// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data

import cats.data.NonEmptyList
import cats.tests.CatsSuite
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.{ FunctorTests, TraverseTests }
import cats.laws.discipline.arbitrary._
import monocle.law.discipline.TraversalTests
import lucuma.core.data.arb.ArbZipper._

/**
  * Tests the Zipper typeclasses
  */
final class ZipperSpec extends CatsSuite {
  test("support modify") {
    forAll { (l: List[Int]) =>
      val z = Zipper.fromNel(NonEmptyList(0, l))
      assert(z.modify(_ => 1) !== z)
    }
  }
  test("Zipper always has at least one item") {
    forAll { (l: Zipper[Int]) =>
      assert(l.length > 0)
    }
  }
  test("supports previous") {
    // move three positions to the right
    val z1 =
      Zipper.fromNel(NonEmptyList(0, List(1, 2, 3, 4, 5, 6))).next.flatMap(_.next).flatMap(_.next)
    assert(z1.exists(_.focus === 3))
    assert(z1.flatMap(_.previous).exists(_.focus === 2))
    assert(z1.flatMap(_.previous).exists(_.lefts === List(1, 0)))
    assert(z1.flatMap(_.previous).exists(_.rights === List(3, 4, 5, 6)))
    assert(z1.flatMap(_.previous).flatMap(_.previous).exists(_.focus === 1))
    assert(z1.flatMap(_.previous).flatMap(_.previous).exists(_.lefts === List(0)))
    assert(z1.flatMap(_.previous).flatMap(_.previous).exists(_.rights === List(2, 3, 4, 5, 6)))
  }
  test("supports next") {
    val z1 = Zipper.fromNel(NonEmptyList(0, List(1, 2, 3, 4, 5, 6)))
    assert(z1.focus === 0)
    assert(z1.next.exists(_.focus === 1))
    assert(z1.next.exists(_.lefts === List(0)))
    assert(z1.next.exists(_.rights === List(2, 3, 4, 5, 6)))
    assert(z1.next.flatMap(_.next).exists(_.focus === 2))
    assert(z1.next.flatMap(_.next).exists(_.lefts === List(1, 0)))
    assert(z1.next.flatMap(_.next).exists(_.rights === List(3, 4, 5, 6)))
  }
  test("previous/next cancel each other") {
    forAll { (l: Zipper[Int]) =>
      if (l.lefts.nonEmpty)
        assert(l.previous.flatMap(_.next) === l.some)
      if (l.rights.nonEmpty)
        assert(l.next.flatMap(_.previous) === l.some)
    }
  }
  test("of") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(Zipper.of(nel.head, nel.tail: _*) === Zipper.fromNel(nel))
    }
  }
  test("toNel") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(Zipper.fromNel(nel).toNel === nel)
    }
  }
  test("toList") {
    forAll { (h: Int, l: List[Int]) =>
      assert(Zipper.fromNel(NonEmptyList(h, l)).toList === h :: l)
    }
  }
  test("support exists") {
    forAll { (l: List[Int]) =>
      val u = Zipper.fromNel(NonEmptyList(0, l))
      val e = u.exists(_ === 0)
      assert(e)
    }
  }
  test("support find") {
    forAll { (r: List[Int]) =>
      val u = Zipper.fromNel(NonEmptyList(0, r))
      val e = u.find(_ === 0)
      assert(e.isDefined)
    }
  }
  test("find focus on focus") {
    // move three positions to the right
    val z1 =
      Zipper.fromNel(NonEmptyList(0, List(1, 2, 3, 4, 5, 6))).next.flatMap(_.next).flatMap(_.next)
    assert(z1.flatMap(_.findFocus(_ === 3)).exists(_.focus === 3))
    assert(z1.flatMap(_.findFocus(_ === 3)).exists(_.lefts === List(2, 1, 0)))
    assert(z1.flatMap(_.findFocus(_ === 3)).exists(_.rights == List(4, 5, 6)))
  }
  test("find focus on lefts") {
    // move three positions to the right
    val z1 =
      Zipper.fromNel(NonEmptyList(0, List(1, 2, 3, 4, 5, 6))).next.flatMap(_.next).flatMap(_.next)
    assert(z1.flatMap(_.findFocus(_ === 0)).exists(_.focus === 0))
    assert(z1.flatMap(_.findFocus(_ === 0)).exists(_.lefts.isEmpty))
    assert(z1.flatMap(_.findFocus(_ === 0)).exists(_.rights == List(1, 2, 3, 4, 5, 6)))
    assert(z1.flatMap(_.findFocus(_ === 1)).exists(_.focus === 1))
    assert(z1.flatMap(_.findFocus(_ === 1)).exists(_.lefts === List(0)))
    assert(z1.flatMap(_.findFocus(_ === 1)).exists(_.rights === List(2, 3, 4, 5, 6)))
    assert(z1.flatMap(_.findFocus(_ === 2)).exists(_.focus === 2))
    assert(z1.flatMap(_.findFocus(_ === 2)).exists(_.lefts === List(1, 0)))
    assert(z1.flatMap(_.findFocus(_ === 2)).exists(_.rights === List(3, 4, 5, 6)))
  }
  test("find focus on rights") {
    // move three positions to the right
    val z1 =
      Zipper.fromNel(NonEmptyList(0, List(1, 2, 3, 4, 5, 6))).next.flatMap(_.next).flatMap(_.next)
    assert(z1.flatMap(_.findFocus(_ === 4)).exists(_.focus === 4))
    assert(z1.flatMap(_.findFocus(_ === 4)).exists(_.lefts === List(3, 2, 1, 0)))
    assert(z1.flatMap(_.findFocus(_ === 4)).exists(_.rights == List(5, 6)))
    assert(z1.flatMap(_.findFocus(_ === 5)).exists(_.focus === 5))
    assert(z1.flatMap(_.findFocus(_ === 5)).exists(_.lefts === List(4, 3, 2, 1, 0)))
    assert(z1.flatMap(_.findFocus(_ === 5)).exists(_.rights === List(6)))
    assert(z1.flatMap(_.findFocus(_ === 6)).exists(_.focus === 6))
    assert(z1.flatMap(_.findFocus(_ === 6)).exists(_.lefts === List(5, 4, 3, 2, 1, 0)))
    assert(z1.flatMap(_.findFocus(_ === 6)).exists(_.rights.isEmpty))
  }
  test("support find focus I") {
    forAll { (r: List[Int]) =>
      val u = Zipper.fromNel(NonEmptyList(0, 1 :: r))
      val e = u.findFocus(_ === 1)
      assert(e.exists(_.focus === 1))
    }
  }
  test("support find focus II") {
    forAll { (l: List[Int]) =>
      val u = Zipper.fromNel(NonEmptyList(0, l))
      val e = u.findFocus(x => l.headOption.forall(x === _))
      val m = l.headOption.forall(x => e.exists(_.focus === x))
      assert(m)
    }
  }
  checkAll("Functor[Zipper]", FunctorTests[Zipper].functor[Int, Int, Int])
  checkAll("Traversable[Zipper]",
           TraverseTests[Zipper].traverse[Int, Int, Int, Int, Option, Option]
  )
  checkAll("Eq[Zipper]", EqTests[Zipper[Int]].eqv)
  checkAll("Zipper.zipperT", TraversalTests(Zipper.zipperT[Int]))
}
