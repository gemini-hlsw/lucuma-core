// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import cats.mtl.Stateful
import cats.Monad
import cats.syntax.all.*
import lucuma.core.model.Semester
import lucuma.core.model.ProposalReference
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Target
import eu.timepit.refined.types.numeric.PosLong

trait Namer[F[_]]:
  def nextProposalReference(sem: Semester): F[ProposalReference]
  def targetIdForString(string: String): F[Target.Id]

object Namer:

  case class State(currProposalIndex: Int, targetStrings: List[String]):
    def advanceProposalIndex = copy(currProposalIndex = currProposalIndex + 1)
    def addTargetString(s: String) = copy(targetStrings = targetStrings :+ s)

  object State:
    val Initial = State(0, Nil)

  def apply[F[_]: Monad](using S: Stateful[F, State]): Namer[F] =
    import S.{ modify, inspect, get }
    new Namer[F]:

      def targetIdForString(s: String): F[Target.Id] =
        get.flatMap: state =>
          state.targetStrings.indexOf(s) match
            case -1 => modify(_.addTargetString(s)) >> targetIdForString(s)
            case n => Target.Id(PosLong.unsafeFrom(n + 1)).pure         

      def nextProposalReference(sem: Semester): F[ProposalReference] =
        modify(s => s.advanceProposalIndex) >>
        inspect(s => ProposalReference(sem, PosInt.unsafeFrom(s.currProposalIndex)))
