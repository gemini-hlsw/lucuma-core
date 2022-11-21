// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.state

import cats.Eval
import cats.Now
import cats.data.IndexedStateT
import cats.data.State
import monocle.PLens

trait StateLensSyntax {
  implicit def toStateLensOps[S, T, A, B](lens: PLens[S, T, A, B]): StateLensOps[S, T, A, B] =
    new StateLensOps[S, T, A, B](lens)
}

final class StateLensOps[S, T, A, B](private val lens: PLens[S, T, A, B]) extends AnyVal {

  /** transforms a PLens into a State */
  def toState: State[S, A] =
    State(s => (s, lens.get(s)))

  /** alias for toState */
  def st: State[S, A] =
    toState

  /** extracts the value viewed through the lens */
  def extract: State[S, A] =
    toState

  /** extracts the value viewed through the lens and applies `f` over it */
  def extracts[D](f: A => D): State[S, D] =
    extract.map(f)
  
  /** modify the value viewed through the lens and returns its *new* value */
  def mod(f: A => B): IndexedStateT[Eval, S, T, B] =
    IndexedStateT { s =>
      val a = lens.get(s)
      val b = f(a)
      Now((lens.replace(b)(s), b))
    }

  /** modify the value viewed through the lens and returns its *old* value */
  def modo(f: A => B): IndexedStateT[Eval, S, T, A] =
    toState.bimap(lens.modify(f), identity)

  /** modify the value viewed through the lens and ignores both values */
  def mod_(f: A => B): IndexedStateT[Eval, S, T, Unit] =
    IndexedStateT(s => Now((lens.modify(f)(s), ())))

  /** set the value viewed through the lens and returns its *new* value */
  def assign(b: B): IndexedStateT[Eval, S, T, B] =
    mod(_ => b)

  /** set the value viewed through the lens and returns its *old* value */
  def assigno(b: B): IndexedStateT[Eval, S, T, A] =
    modo(_ => b)

  /** set the value viewed through the lens and ignores both values */
  def assign_(b: B): IndexedStateT[Eval, S, T, Unit] =
    mod_(_ => b)
}
