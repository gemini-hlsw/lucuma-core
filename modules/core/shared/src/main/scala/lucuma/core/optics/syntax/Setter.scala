// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.syntax

import cats.data.State
import lucuma.core.optics.state.all._
import monocle.Setter

final class SetterOps[S, A](private val self: Setter[S, A]) extends AnyVal {

  def edit(a: A): State[S, Unit] =
    self.assign_(a)

  @inline def :=(a: A): State[S, Unit] =
    edit(a)

  def edit(a: Option[A]): State[S, Unit] =
    a.fold(State.pure[S, Unit](()))(self.assign_(_))

  @inline def :=(a: Option[A]): State[S, Unit] =
    edit(a)

}

trait ToSetterOps {
  implicit def ToSetterOps[S, A](s: Setter[S, A]): SetterOps[S, A] =
    new SetterOps(s)
}

object setter extends ToSetterOps
