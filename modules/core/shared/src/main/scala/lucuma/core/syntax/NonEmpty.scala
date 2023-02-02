// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import cats.Order
import cats.data.NonEmptyList
import cats.syntax.foldable.*
import cats.syntax.order.*
import lucuma.core.data.Zipper

import scala.annotation.tailrec

trait NonEmptyOps {

  extension [A](self: NonEmptyList[A]) {

    /**
     * Creates a Zipper from a NonEmptyList.  The first element of the NEL
     * becomes the focus.
     */
    def toZipper: Zipper[A] =
      Zipper.fromNel(self)

    /**
     * Creates a Zipper from the NonEmptyList where the focused element is the
     * minimum according to the `Order` instance for `A`.
     */
    def toZipperMin(implicit ev: Order[A]): Zipper[A] =
      focusCompare(_ < _)

    /**
     * Creates a Zipper from the NonEmptyList where the focused element is the
     * maximum according to the `Order` instance for `A`.
     */
    def toZipperMax(implicit ev: Order[A]): Zipper[A] =
      focusCompare(_ > _)

    private def focusCompare(f: (A, A) => Boolean) = {

      @tailrec
      def go(cur: Zipper[A], res: Zipper[A]): Zipper[A] =
        cur.next match {
          case None    => res
          case Some(n) => if (f(n.focus, res.focus)) go(n, n) else go(n, res)
        }

      val init = Zipper.fromNel(self)
      go(init, init)
    }

  }

}

object nonempty extends NonEmptyOps