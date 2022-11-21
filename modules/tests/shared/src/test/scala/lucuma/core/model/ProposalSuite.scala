// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order._
import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import lucuma.core.model.Proposal
import lucuma.core.model.arb._
import lucuma.core.util.arb.ArbCollection
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline._
import munit._
import org.scalacheck.Cogen._

class ProposalSuite extends DisciplineSuite {
  import ArbCollection._
  import ArbEnumerated._
  import ArbProposal._
  import ArbProposalClass._

  checkAll("Eq[Proposal]", EqTests[Proposal].eqv)

  checkAll("Proposal.title", LensTests(Proposal.title))
  checkAll("Proposal.proposalClass", LensTests(Proposal.proposalClass))
  checkAll("Proposal.category", LensTests(Proposal.category))
  checkAll("Proposal.toOActivation", LensTests(Proposal.toOActivation))
  checkAll("Proposal.abstrakt", LensTests(Proposal.abstrakt))
  checkAll("Proposal.partnerSplits", LensTests(Proposal.partnerSplits))
}
