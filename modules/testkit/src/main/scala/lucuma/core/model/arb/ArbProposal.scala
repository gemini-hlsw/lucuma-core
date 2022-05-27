// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.implicits._
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.`enum`.{ TacCategory, ToOActivation }
import lucuma.core.model.{ IntPercent, Partner, Proposal, ProposalClass }
import lucuma.core.util.arb.{ ArbCollection, ArbEnumerated }
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbProposal {
  import ArbCollection._
  import ArbEnumerated._
  import ArbProposalClass._

  implicit val arbProposal: Arbitrary[Proposal] =
    Arbitrary {
      for {
        title    <- arbitrary[Option[NonEmptyString]]
        pClass   <- arbitrary[ProposalClass]
        category <- arbitrary[Option[TacCategory]]
        too      <- arbitrary[ToOActivation]
        abstrakt <- arbitrary[Option[NonEmptyString]]
        splits   <- arbitrary[SortedMap[Partner, IntPercent]]
      } yield Proposal(title, pClass, category, too, abstrakt, splits)
    }

  implicit val cogProposal: Cogen[Proposal] =
    Cogen[
      (Option[NonEmptyString],
       ProposalClass,
       Option[TacCategory],
       ToOActivation,
       Option[NonEmptyString],
       SortedMap[Partner, IntPercent]
      )
    ].contramap(p =>
      (p.title, p.proposalClass, p.category, p.toOActivation, p.abstrakt, p.partnerSplits)
    )
}

object ArbProposal extends ArbProposal
