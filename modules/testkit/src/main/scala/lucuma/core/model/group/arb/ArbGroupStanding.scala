// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group
package arb

import lucuma.core.enums.GroupStandingStatus
import lucuma.core.model.Group
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import GroupStanding.*

trait ArbGroupStanding:
  import ArbEnumerated.given
  import ArbGid.given

  given Arbitrary[GroupStanding] =
    Arbitrary:
      for
        g <- arbitrary[Group.Id]
        s <- Gen.oneOf(Viable, Complete(g), Unschedulable(g))
      yield s

  given Cogen[GroupStanding] =
    Cogen[(GroupStandingStatus, Option[Group.Id])].contramap:
      case Viable           => (GroupStandingStatus.Viable, None)
      case Complete(g)      => (GroupStandingStatus.Complete, Some(g))
      case Unschedulable(g) => (GroupStandingStatus.Unschedulable, Some(g))

object ArbGroupStanding extends ArbGroupStanding
