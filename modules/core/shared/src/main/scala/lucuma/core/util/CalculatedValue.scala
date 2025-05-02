// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Monad
import cats.Monoid
import cats.Semigroup
import cats.Traverse
import cats.kernel.CommutativeMonoid
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.order.*

import scala.annotation.tailrec

/**
 * A value that is automatically maintained by a background worker.  The
 * underlying value is associated with a calculation state. If the state is
 * 'Ready' then the value may be considered up-to-date at the moment it was
 * retrieved. Otherwise the value represents the last known value before an
 * update (possibly) invalidated it and triggered a new computation.
 *
 * @param state current calculation state
 * @param value last known underlying value
 * @tparam A data type
 */
final case class CalculatedValue[A](
  state: CalculationState,
  value: A
)

object CalculatedValue:

  /**
   * An empty, or zero, 'Ready' calculated value.
   */
  def empty[A](using Monoid[A]): CalculatedValue[A] =
    CalculatedValue(CalculationState.Zero, Monoid[A].empty)

  given [A: Eq]: Eq[CalculatedValue[A]] =
    Eq.by(cv => (cv.state, cv.value))

  private def combine[A: Semigroup](a: CalculatedValue[A], b: CalculatedValue[A]): CalculatedValue[A] =
    CalculatedValue(a.state |+| b.state, a.value |+| b.value)

  given [A](using CommutativeMonoid[A]): CommutativeMonoid[CalculatedValue[A]] =
    CommutativeMonoid.instance[CalculatedValue[A]](empty, combine)

  given [A](using Monoid[A]): Monoid[CalculatedValue[A]] =
    Monoid.instance[CalculatedValue[A]](empty, combine)

  given [A](using Semigroup[A]): Semigroup[CalculatedValue[A]] =
    Semigroup.instance[CalculatedValue[A]](combine)

  given Monad[CalculatedValue] with Traverse[CalculatedValue] with
    def pure[A](a: A): CalculatedValue[A] =
      CalculatedValue(CalculationState.Zero, a)

    def flatMap[A, B](fa: CalculatedValue[A])(f: A => CalculatedValue[B]): CalculatedValue[B] =
      val fb = f(fa.value)
      CalculatedValue(fa.state |+| fb.state, fb.value)

    def tailRecM[A, B](a: A)(f: A => CalculatedValue[Either[A, B]]): CalculatedValue[B] =
      @tailrec def loop(p: CalculationState, a: A): CalculatedValue[B] =
        val result = f(a)
        val phase  = p |+| result.state
        result.value match
          case Left(nextA) => loop(phase, nextA)
          case Right(b)    => CalculatedValue(phase, b)

      loop(CalculationState.Zero, a)

    def traverse[G[_]: Applicative, A, B](fa: CalculatedValue[A])(f: A => G[B]): G[CalculatedValue[B]] =
      f(fa.value).map(b => CalculatedValue(fa.state, b))

    def foldLeft[A, B](fa: CalculatedValue[A], b: B)(f: (B, A) => B): B =
      f(b, fa.value)

    def foldRight[A, B](fa: CalculatedValue[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.value, lb)