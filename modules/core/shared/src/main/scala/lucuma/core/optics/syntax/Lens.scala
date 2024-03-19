// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.syntax

import cats.data.State
import lucuma.core.optics.state.all.*
import monocle.Lens

final class LensOps[S, A](private val self: Lens[S, A]) extends AnyVal {
  def edit(a: A): State[S, A] =
    self.assign(a)

  @inline def :=(a: A): State[S, A] =
    edit(a)

  def edit(a: Option[A]): State[S, A] =
    a.fold(self.st)(self.assign)

  @inline def :=(a: Option[A]): State[S, A] =
    edit(a)
}

trait ToLensOps {
  implicit def ToLensOps[S, B](l: Lens[S, B]): LensOps[S, B] =
    new LensOps(l)
}

object lens extends ToLensOps
