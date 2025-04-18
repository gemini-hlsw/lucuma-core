// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all.*
import monocle.Focus
import monocle.Lens

/**
 * An ExecutionSequence is a logically a list of Atom.  It includes a concrete
 * `nextAtom` that should be executed and a `possibleFuture` list of atoms
 * which may be generated in the future.  It is important to note that the
 * `possibleFuture` may not be complete.  Lengthy future atom lists are
 * truncated to a reasonably small size.
 *
 * @tparam D dynamic config type
 *
 * @param nextAtom       next atom to be executed
 * @param possibleFuture initial atoms that may appear in the future (this may
 *                       not include all potential future atoms that are known
 *                       at the time of creation)
 * @param hasMore        whether the `possibleFuture` is incomplete
 */
case class ExecutionSequence[D](
  nextAtom:       Atom[D],
  possibleFuture: List[Atom[D]],
  hasMore:        Boolean
)

object ExecutionSequence {

  /** @group Optics */
  def nextAtom[D]: Lens[ExecutionSequence[D], Atom[D]] =
    Focus[ExecutionSequence[D]](_.nextAtom)

  /** @group Optics */
  def possibleFuture[D]: Lens[ExecutionSequence[D], List[Atom[D]]] =
    Focus[ExecutionSequence[D]](_.possibleFuture)

  /** @group Optics */
  def hasMore[D]: Lens[ExecutionSequence[D], Boolean] =
    Focus[ExecutionSequence[D]](_.hasMore)

  given [D](using Eq[D]): Eq[ExecutionSequence[D]] =
    Eq.by { a => (
      a.nextAtom,
      a.possibleFuture,
      a.hasMore
    )}

}

