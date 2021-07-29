// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.syntax

import cats.data.State
import lucuma.core.optics.state.all._
import monocle.Optional

final class OptionalOps[S, A](val self: Optional[S, A]) extends AnyVal {

  def edit(a: A): State[S, Option[A]] =
    self.assign(a)

  @inline def :=(a: A): State[S, Option[A]] =
    edit(a)

  def edit(a: Option[A]): State[S, Option[A]] =
    a.fold(self.st)(self.assign)

  @inline def :=(a: Option[A]): State[S, Option[A]] =
    edit(a)

}

trait ToOptionalOps {
  implicit def ToOptionalOps[S, A](opt: Optional[S, A]): OptionalOps[S, A] =
    new OptionalOps[S, A](opt)
}

object optional extends ToOptionalOps
