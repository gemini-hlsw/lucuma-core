// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import lucuma.core.optics.laws.discipline.*
import lucuma.core.model.arb.ArbProposalReference

final class ProposalReferenceSuite extends munit.DisciplineSuite {

  import ArbProposalReference.given
  import ArbProposalReference.proposalReferenceStrings

  checkAll("ProposalReference", OrderTests[ProposalReference].order)
  checkAll("ProposalReference.fromString", FormatTests(ProposalReference.fromString).formatWith(proposalReferenceStrings))

}
