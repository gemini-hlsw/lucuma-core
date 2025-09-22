// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics.syntax

import cats.data.State
import lucuma.core.optics.state.all.*
import monocle.Iso
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

trait DisjointZip {
  extension [S, A](l1: Lens[S, A])
    // Lenses must be disjoint (not overlap), or the result will be unsafe.
    // See https://github.com/optics-dev/Monocle/issues/545
    def disjointZip[B](l2: Lens[S, B]): Lens[S, (A, B)] =
      Lens((s: S) => (l1.get(s), l2.get(s)))((ab: (A, B)) =>
        (s: S) => l2.replace(ab._2)(l1.replace(ab._1)(s))
      )

  extension [S, A, B](lenses: (Lens[S, A], Lens[S, B]))
    def disjointZip: Lens[S, (A, B)] = lenses._1.disjointZip(lenses._2)

  extension [S, A, B, C](lenses: (Lens[S, A], Lens[S, B], Lens[S, C]))
    def disjointZip: Lens[S, (A, B, C)] =
      ((lenses._1, lenses._2).disjointZip, lenses._3).disjointZip
        .andThen(Iso[((A, B), C), (A, B, C)] { case ((a, b), c) => (a, b, c) } { case (a, b, c) =>
          ((a, b), c)
        })

  extension [S, A, B, C, D](lenses: (Lens[S, A], Lens[S, B], Lens[S, C], Lens[S, D]))
    def disjointZip: Lens[S, (A, B, C, D)] =
      ((lenses._1, lenses._2, lenses._3).disjointZip, lenses._4).disjointZip
        .andThen(Iso[((A, B, C), D), (A, B, C, D)] { case ((a, b, c), d) => (a, b, c, d) } {
          case (a, b, c, d) => ((a, b, c), d)
        })

  extension [S, A, B, C, D, E](lenses: (Lens[S, A], Lens[S, B], Lens[S, C], Lens[S, D], Lens[S, E]))
    def disjointZip: Lens[S, (A, B, C, D, E)] =
      ((lenses._1, lenses._2, lenses._3, lenses._4).disjointZip, lenses._5).disjointZip
        .andThen(Iso[((A, B, C, D), E), (A, B, C, D, E)] { case ((a, b, c, d), e) =>
          (a, b, c, d, e)
        } { case (a, b, c, d, e) =>
          ((a, b, c, d), e)
        })
}

object lens extends ToLensOps with DisjointZip
