// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.syntax

import cats.data.State
import monocle.Lens
import monocle.state.all._

final class LensEditorOps[S, A](val self: Lens[S, A]) extends AnyVal {
  def edit(a: A): State[S, A] =
    self.assign(a)

  @inline def :=(a: A): State[S, A] =
    edit(a)

  def edit(a: Option[A]): State[S, A] =
    a.fold(self.st)(self.assign)

  @inline def :=(a: Option[A]): State[S, A] =
    edit(a)
}

trait ToLensEditorOps {
  implicit def ToLensOps[S, B](l: Lens[S, B]): LensEditorOps[S, B] =
    new LensEditorOps(l)
}

object lens extends ToLensEditorOps
