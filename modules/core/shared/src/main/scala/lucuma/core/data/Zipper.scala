// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package data

import cats.*
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import monocle.Prism
import monocle.Traversal

import scala.annotation.tailrec

/**
  * Minimal zipper based on scalaz's implementation
  * This is only meant for small collections. performance has not been optimized
  */
protected[data] trait ZipperOps[A, +Z] {
  val lefts: List[A]
  val focus: A
  val rights: List[A]

  protected def build(lefts: List[A], focus: A, rights: List[A]): Z

  protected def unmodified: Z

  /**
    * Find and element and focus if successful
    */
  def findFocusP(p: PartialFunction[A, Boolean]): Option[Z] =
    findFocus(p.lift.andThen(_.getOrElse(false)))

  /**
    * How many items are in the zipper
    */
  def length: Int = lefts.length + 1 + rights.length

  /**
    * Find and element and focus if successful
    */
  def findFocus(p: A => Boolean): Option[Z] =
    if (p(focus)) unmodified.some
    else {
      val indexLeft  = lefts.lastIndexWhere(p)
      val indexRight = rights.indexWhere(p)
      if (indexLeft === -1 && indexRight === -1)
        none
      else if (indexLeft >= 0)
        (lefts.splitAt(indexLeft) match {
          case (x, i :: l) =>
            build(l, i, (focus :: x).reverse ::: rights)
          case _           =>
            unmodified
        }).some
      else
        (rights.splitAt(indexRight) match {
          case (x, h :: t) =>
            build((focus :: x).reverse ::: lefts, h, t)
          case _           =>
            unmodified
        }).some
    }

  def focusFirst: Z = {
    val h :: t = toList : @unchecked
    build(Nil, h, t)
  }

  /**
   * Returns the index of the focused element.  In other words, the index of
   * the focused element if the Zipper were exported `toList`.
   */
  def indexOfFocus: Int =
    lefts.size

  /**
   * Focuses on the given element index, if in range.
   * @param index index of the element to focus
   * @return a Some(Zipper) which is the same as this Zipper but with the
   *         indicated element as the focus, or else None if the index is out
   *         of range
   */
  def focusIndex(index: Int): Option[Z] = {
    val leftLen  = lefts.length
    val rightLen = rights.length

    if (index < 0) {
      none

    } else if (index < leftLen) {
      val (x, newLeft) = lefts.splitAt(leftLen - index)
      val newFocus :: newRight = (focus :: x).reverse ::: rights : @unchecked
      build(newLeft, newFocus, newRight).some

    } else if (index === leftLen) {
      unmodified.some

    } else if (index < (leftLen + 1 + rightLen)) {
      val (x, newFocus :: newRight) = rights.splitAt(index - leftLen - 1) : @unchecked
      build((focus :: x).reverse ::: lefts, newFocus, newRight).some

    } else {
      none
    }
  }

  /**
   * Focuses on the first element (left to right) which is a maximum element
   * in the zipper according to Order[A].
   */
  def focusMax(using Order[A]): Z =
    focusCompare(_ > _)

  /**
   * Focuses on the first element (left to right) which is a minimum element
   * in the zipper according to Order[A].
   */
  def focusMin(using Order[A]): Z =
    focusCompare(_ < _)

  private def focusCompare(f: (A, A) => Boolean): Z = {

    @tailrec
    def go(
      cur: (List[A], A, List[A]),
      res: (List[A], A, List[A])
    ): (List[A], A, List[A]) =
      cur._3 match {
        case Nil              =>
          res
        case hRight :: tRight =>
          val next = (cur._2 :: cur._1, hRight, tRight)
          if (f(hRight, res._2)) go(next, next) else go(next, res)
      }

    // focus first
    val h :: t = toList : @unchecked
    val init   = (List.empty[A], h, t)

    build.tupled(go(init, init))
  }

  def exists(p: A => Boolean): Boolean =
    if (p(focus))
      true
    else
      lefts.exists(p) || rights.exists(p)

  def previous: Option[Z] =
    lefts match {
      case Nil    => none
      case h :: t => build(t, h, focus :: rights).some
    }

  def next: Option[Z] =
    rights match {
      case Nil       => none
      case h :: tail => build(focus :: lefts, h, tail).some
    }

  def find(p: A => Boolean): Option[A] =
    if (p(focus))
      focus.some
    else
      lefts.find(p).orElse(rights.find(p))

  def withFocus: Zipper[(A, Boolean)] =
    Zipper(lefts.map((_, false)), (focus, true), rights.map((_, false)))

  def toList: List[A] = lefts.reverse ::: (focus :: rights)

  def toNel: NonEmptyList[A] = NonEmptyList.fromListUnsafe(toList)

  override def toString: String =
    s"Zipper(${lefts.mkString("(", ", ", ")")}, $focus, ${rights.mkString("(", ", ", ")")})"
}

class Zipper[A] protected (val lefts: List[A], val focus: A, val rights: List[A])
    extends ZipperOps[A, Zipper[A]] {

  override protected def build(lefts: List[A], focus: A, rights: List[A]): Zipper[A] =
    Zipper.build(lefts, focus, rights)

  override protected def unmodified: Zipper[A] = this

  /**
    * Modify the focus
    */
  def modify(f: A => A): Zipper[A] = build(lefts, f(focus), rights)

  /**
    * Modify the focus
    */
  def modifyP(p: Prism[A, A]): Zipper[A] =
    p.getOption(focus).map(f => build(lefts, f, rights)).getOrElse(unmodified)

  protected def canEqual(other: Any): Boolean = other.isInstanceOf[Zipper[?]]

  override def equals(other: Any): Boolean = other match
    case that: Zipper[?] =>
      that.canEqual(this) &&
        lefts == that.lefts &&
        focus == that.focus &&
        rights == that.rights
    case _ => false

  override def hashCode(): Int =
    val state = Seq(lefts, focus, rights)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}

object Zipper extends ZipperFactory[Zipper] {

  def apply[A](lefts: List[A], focus: A, rights: List[A]): Zipper[A] =
    new Zipper(lefts, focus, rights)

  /**
   * Builds a Zipper from a List, if it is non-empty.  The head (if any)
   * becomes the focus.
   */
  def fromList[A](as: List[A]): Option[Zipper[A]] =
    NonEmptyList.fromList(as).map(fromNel)

  /**
   * Builds a Zipper from NonEmptyList. The head of the list becomes the focus
   */
  def fromNel[A](ne: NonEmptyList[A]): Zipper[A] =
    apply(Nil, ne.head, ne.tail)

  /**
    * Builds a Zipper from elements. The first element becomes the focus
    */
  def of[A](a: A, as: A*): Zipper[A] =
    apply(Nil, a, as.toList)

  /**
   * Builds a Zipper with only one element, which of course is focused.
   */
  def one[A](a: A): Zipper[A] =
    apply(Nil, a, Nil)

  protected def build[A](lefts: List[A], focus: A, rights: List[A]): Zipper[A] =
    apply(lefts, focus, rights)

  /**
   * Based on traverse implementation for List
   * @group Typeclass Instances
   */
  given Traverse[Zipper] = new Traverse[Zipper] {
    override def traverse[G[_], A, B](
      fa: Zipper[A]
    )(f:  A => G[B])(using Applicative[G]): G[Zipper[B]] =
      (fa.lefts.traverse(f), f(fa.focus), fa.rights.traverse(f)).mapN {
        case (l, f, r) => build(l, f, r)
      }

    override def foldLeft[A, B](fa: Zipper[A], b: B)(f: (B, A) => B): B =
      fa.toNel.foldLeft(b)(f)

    override def foldRight[A, B](fa: Zipper[A], lb: Eval[B])(
      f:                             (A, Eval[B]) => Eval[B]
    ): Eval[B] = {
      def loop(as: Vector[A]): Eval[B] =
        as match {
          case h +: t => f(h, Eval.defer(loop(t)))
          case _      => lb
        }

      Eval.defer(loop(fa.toList.toVector))
    }
  }

  /**
    * Creates a monocle Traversal for the Zipper
    */
  def zipperT[A]: Traversal[Zipper[A], A] =
    Traversal.fromTraverse

  /**
    * Traversal filtered zipper, Note this is unsafe as the predicate breaks some laws
    */
  def unsafeSelect[A](predicate: A => Boolean): Traversal[Zipper[A], A] =
    new Traversal[Zipper[A], A] {
      override def modifyA[F[_]: Applicative](f: A => F[A])(s: Zipper[A]): F[Zipper[A]] = {
        val lefts: F[List[A]]  = s.lefts.traverse {
          case x if predicate(x) => f(x)
          case x                 => x.pure[F]
        }
        val rights: F[List[A]] = s.rights.traverse {
          case x if predicate(x) => f(x)
          case x                 => x.pure[F]
        }
        val focus: F[A]        =
          if (predicate(s.focus)) f(s.focus) else s.focus.pure[F]
        (lefts, focus, rights).mapN { (l, f, r) =>
          build(l, f, r)
        }
      }
    }
}

// Where a Zipper is represented as a type with `selected: A` and `all: [A!]!`
// fields.

trait ZipperCodec {

  given [A: Decoder : Eq]: Decoder[Zipper[A]] =
    Decoder.instance { c =>
      for
        list   <- c.downField("all").as[List[A]]
        idx    <- c.downField("index").as[Int] orElse
                    c.downField("selected").as[A].flatMap(a =>
                      list
                        .indexWhere(_ === a)
                        .some
                        .filter(_ >= 0)
                        .toRight(DecodingFailure("The `selected` item is not a member of the `all` list of items.", c.history))
                      )
        zipper <- Zipper.fromList(list)
                    .toRight(DecodingFailure("The `all` list of items must be non-empty.", c.history))
                    .flatMap(
                      _.focusIndex(idx)
                        .toRight(DecodingFailure("The `index` value is out of bounds.", c.history))
                    )
      yield zipper
    }

  given [A: Encoder]: Encoder[Zipper[A]] =
    Encoder.instance { (a: Zipper[A]) =>
      Json.obj(
        "selected" -> a.focus.asJson,
        "index"    -> a.indexOfFocus.asJson,
        "all"      -> a.toList.asJson
      )
    }

}

object ZipperCodec extends ZipperCodec
