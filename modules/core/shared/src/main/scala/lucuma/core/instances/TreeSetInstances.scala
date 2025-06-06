// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.instances

import cats.*
import cats.kernel.BoundedSemilattice
import cats.kernel.Hash
import cats.kernel.Order
import cats.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

trait TreeSetInstances extends TreeSetInstances1 {

  // For some reason this type can't be inlined
  private type TreeSetInstancesType = Foldable[TreeSet] & SemigroupK[TreeSet]

  given TreeSetInstancesType =
    new Foldable[TreeSet] with SemigroupK[TreeSet] {

      def combineK[A](x: TreeSet[A], y: TreeSet[A]): TreeSet[A] = x | y

      def foldLeft[A, B](fa: TreeSet[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: TreeSet[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)

      override def foldMap[A, B](fa: TreeSet[A])(f: A => B)(using B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override def get[A](fa: TreeSet[A])(idx: Long): Option[A] = {
        @tailrec
        def go(idx: Int, it: Iterator[A]): Option[A] =
          if (it.hasNext)
            if (idx == 0) Some(it.next())
            else {
              it.next()
              go(idx - 1, it)
            }
          else None
        if (idx < Int.MaxValue && idx >= 0L) go(idx.toInt, fa.iterator) else None
      }

      override def size[A](fa: TreeSet[A]): Long = fa.size.toLong

      override def exists[A](fa: TreeSet[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: TreeSet[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: TreeSet[A]): Boolean = fa.isEmpty

      override def fold[A](fa: TreeSet[A])(using A: Monoid[A]): A = A.combineAll(fa)

      override def toList[A](fa: TreeSet[A]): List[A] = fa.toList

      override def reduceLeftOption[A](fa: TreeSet[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override def find[A](fa: TreeSet[A])(f: A => Boolean): Option[A] = fa.find(f)

      override def collectFirst[A, B](fa: TreeSet[A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: TreeSet[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))
    }

  given [A: Show]: Show[TreeSet[A]] =
    new Show[TreeSet[A]] {
      def show(fa: TreeSet[A]): String =
        fa.iterator.map(_.show).mkString("TreeSet(", ", ", ")")
    }

  given [A: Order]: Order[TreeSet[A]] =
    new TreeSetOrder[A]
}

trait TreeSetInstances1 {
  given [A: Order: Hash]: Hash[TreeSet[A]] =
    new TreeSetHash[A]

  given [A: Order]: BoundedSemilattice[TreeSet[A]] =
    new TreeSetSemilattice[A]
}

class TreeSetOrder[A: Order] extends Order[TreeSet[A]] {
  def compare(a1: TreeSet[A], a2: TreeSet[A]): Int =
    Order[Int].compare(a1.size, a2.size) match {
      case 0 => Order.compare(a1.to(LazyList), a2.to(LazyList))
      case x => x
    }

  override def eqv(s1: TreeSet[A], s2: TreeSet[A]): Boolean =
    // implicit val x = Order[A].toOrdering
    s1.to(LazyList).corresponds(s2.to(LazyList))(Order[A].eqv)
}

class TreeSetHash[A: Order: Hash] extends Hash[TreeSet[A]] {
  import scala.util.hashing.MurmurHash3._

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  def hash(xs: TreeSet[A]): Int = {
    var a, b, n = 0
    var c       = 1
    xs.foreach { x =>
      val h = Hash[A].hash(x)
      a += h
      b ^= h
      if (h != 0) c *= h
      n += 1
    }
    var h       = setSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
  override def eqv(s1: TreeSet[A], s2: TreeSet[A]): Boolean =
    // implicit val x = Order[A].toOrdering
    s1.to(LazyList).corresponds(s2.to(LazyList))(Order[A].eqv)
}

class TreeSetSemilattice[A: Order] extends BoundedSemilattice[TreeSet[A]] {
  def empty: TreeSet[A] = TreeSet.empty(using summon[Order[A]].toOrdering)
  def combine(x: TreeSet[A], y: TreeSet[A]): TreeSet[A] = x | y
}

object treeset extends TreeSetInstances
