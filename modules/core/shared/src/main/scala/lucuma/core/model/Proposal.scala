// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order._
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.TacCategory
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import lucuma.refined._
import monocle.Focus

import scala.collection.immutable.SortedMap

final case class Proposal(
  title:         Option[NonEmptyString],
  proposalClass: ProposalClass,
  category:      Option[TacCategory],
  toOActivation: ToOActivation,
  abstrakt:      Option[NonEmptyString],
  partnerSplits: SortedMap[Partner, IntPercent]
)

object Proposal {
  val title         = Focus[Proposal](_.title)
  val proposalClass = Focus[Proposal](_.proposalClass)
  val category      = Focus[Proposal](_.category)
  val toOActivation = Focus[Proposal](_.toOActivation)
  val abstrakt      = Focus[Proposal](_.abstrakt)
  val partnerSplits = Focus[Proposal](_.partnerSplits)

  val Default: Proposal =
    Proposal(None, ProposalClass.Queue(80.refined), None, ToOActivation.None, None, SortedMap.empty)

  implicit val eqProposal: Eq[Proposal] = Eq.instance {
    case (Proposal(t1, pc1, c1, to1, a1, ps1), Proposal(t2, pc2, c2, to2, a2, ps2)) =>
      t1 === t2 && pc1 === pc2 && c1 === c2 && to1 === to2 && a1 === a2 && ps1 === ps2
  }
}
