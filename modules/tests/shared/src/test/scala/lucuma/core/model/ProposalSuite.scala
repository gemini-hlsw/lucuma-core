// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order.*
import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.Proposal
import lucuma.core.model.arb.*
import lucuma.core.util.arb.ArbCollection
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Cogen.*

class ProposalSuite extends DisciplineSuite {
  import ArbCollection.*
  import ArbEnumerated.*
  import ArbProposal.given
  import ArbProposalClass.given

  checkAll("Eq[Proposal]", EqTests[Proposal].eqv)

  checkAll("Proposal.title", LensTests(Proposal.title))
  checkAll("Proposal.proposalClass", LensTests(Proposal.proposalClass))
  checkAll("Proposal.category", LensTests(Proposal.category))
  checkAll("Proposal.toOActivation", LensTests(Proposal.toOActivation))
  checkAll("Proposal.abstrakt", LensTests(Proposal.abstrakt))
  checkAll("Proposal.partnerSplits", LensTests(Proposal.partnerSplits))
}
