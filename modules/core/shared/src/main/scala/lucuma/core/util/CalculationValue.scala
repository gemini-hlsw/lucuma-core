// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Monad
import cats.Monoid
import cats.Traverse
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.order.*
import scala.annotation.tailrec

case class CalculationValue[A](
  phase: CalculationPhase,
  value: A
)

object CalculationValue:

  def empty[A](using Monoid[A]): CalculationValue[A] =
    CalculationValue(CalculationPhase.Zero, Monoid[A].empty)

  given [A: Eq]: Eq[CalculationValue[A]] =
    Eq.by(cv => (cv.phase, cv.value))

  given [A](using Monoid[A]): Monoid[CalculationValue[A]] =
    Monoid.instance[CalculationValue[A]](
      empty,
      (a, b) => CalculationValue(a.phase |+| b.phase, a.value |+| b.value)
    )

  given Monad[CalculationValue] with
    def pure[A](a: A): CalculationValue[A] =
      CalculationValue(CalculationPhase.Zero, a)

    def flatMap[A, B](fa: CalculationValue[A])(f: A => CalculationValue[B]): CalculationValue[B] =
      val fb = f(fa.value)
      CalculationValue(fa.phase |+| fb.phase, fb.value)

    def tailRecM[A, B](a: A)(f: A => CalculationValue[Either[A, B]]): CalculationValue[B] =
      @tailrec def loop(p: CalculationPhase, a: A): CalculationValue[B] =
        val result = f(a)
        val phase  = p |+| result.phase
        result.value match
          case Left(nextA) => loop(phase, nextA)
          case Right(b)    => CalculationValue(phase, b)

      loop(CalculationPhase.Zero, a)

  given Traverse[CalculationValue] with
    def traverse[G[_]: Applicative, A, B](fa: CalculationValue[A])(f: A => G[B]): G[CalculationValue[B]] =
      f(fa.value).map(b => CalculationValue(fa.phase, b))

    def foldLeft[A, B](fa: CalculationValue[A], b: B)(f: (B, A) => B): B =
      f(b, fa.value)

    def foldRight[A, B](fa: CalculationValue[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.value, lb)