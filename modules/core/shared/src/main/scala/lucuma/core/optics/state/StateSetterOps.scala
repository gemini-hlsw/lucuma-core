// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.state

import cats.Eval
import cats.Now
import cats.data.IndexedStateT
import monocle.PSetter

trait StateSetterSyntax {
  implicit def toStateSetterOps[S, T, A, B](
    setter: PSetter[S, T, A, B]
  ): StateSetterOps[S, T, A, B] =
    new StateSetterOps[S, T, A, B](setter)
}

final class StateSetterOps[S, T, A, B](private val setter: PSetter[S, T, A, B]) extends AnyVal {

  /** modify the value referenced through the setter */
  def mod_(f: A => B): IndexedStateT[Eval, S, T, Unit] =
    IndexedStateT(s => Now((setter.modify(f)(s), ())))

  /** set the value referenced through the setter */
  def assign_(b: B): IndexedStateT[Eval, S, T, Unit] =
    mod_(_ => b)
}
