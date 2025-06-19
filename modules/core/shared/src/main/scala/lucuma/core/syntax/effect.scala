// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import cats.FlatMap
import cats.Functor
import cats.data.NonEmptyChain
import cats.effect.Outcome
import cats.effect.Spawn
import cats.syntax.all.*

trait effect:
  extension [F[_]](F: Spawn[F])
    /**
     * Similar to `F.race`, but in case of error in the winner fiber, keeps the other one. Note that
     * if both fibers fail, the last one to fail will be returned and the first error will be masked.
     */
    def raceToSuccess[A, B](fa: F[A], fb: F[B]): F[Either[A, B]] =
      given FlatMap[F] = F
      F.uncancelable: poll =>
        poll(F.racePair(fa, fb)).flatMap: 
          case Left((oc, f))  =>
            oc match
              case Outcome.Succeeded(ra)                   => f.cancel *> ra.map(Left(_))
              case Outcome.Errored(_) | Outcome.Canceled() =>
                f.join.flatMap:
                  case Outcome.Succeeded(rb) => rb.map(Right(_))
                  case Outcome.Errored(eb)   => F.raiseError(eb)
                  case Outcome.Canceled()    => poll(F.canceled) *> F.never
          case Right((f, oc)) =>
            oc match
              case Outcome.Succeeded(rb)                   => f.cancel *> rb.map(Right(_))
              case Outcome.Errored(_) | Outcome.Canceled() =>
                f.join.flatMap:
                  case Outcome.Succeeded(ra) => ra.map(Left(_))
                  case Outcome.Errored(ea)   => F.raiseError(ea)
                  case Outcome.Canceled()    => poll(F.canceled) *> F.never

    /**
     * Runs a list of requests in parallel and returns the first successful result. If all requests
     * fail, the last error will be returned.
     */
    def raceAllToSuccess[A](fs: NonEmptyChain[F[A]]): F[A] =
      given Functor[F] = F
      fs.tail.foldLeft(fs.head)(raceToSuccess(_, _).map(_.merge))              

  extension [F[_], A](fa: F[A])(using F: Spawn[F])
    def raceToSuccess[B](fb: F[B]): F[Either[A, B]] =
      F.raceToSuccess(fa, fb)

  extension [F[_], A](fs: NonEmptyChain[F[A]])(using F: Spawn[F])
    def raceAllToSuccess: F[A] =
      F.raceAllToSuccess(fs)

object effect extends effect
  
