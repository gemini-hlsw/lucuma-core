// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group
package arb

import lucuma.core.model.Attribution
import lucuma.core.model.Group
import lucuma.core.model.arb.ArbAttribution
import lucuma.core.util.arb.ArbGid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbGroupConstraintWaiver:
  import ArbAttribution.given
  import ArbGid.given
  import ArbGroupConstraintViolation.given

  given Arbitrary[GroupConstraintWaiver] =
    Arbitrary:
      for
        id <- arbitrary[GroupConstraintWaiver.Id]
        g  <- arbitrary[Group.Id]
        v  <- arbitrary[GroupConstraintViolation.Waivable]
        a  <- arbitrary[Attribution]
        r  <- arbitrary[Option[Attribution]]
      yield GroupConstraintWaiver(id, g, v, a, r)

  given Cogen[GroupConstraintWaiver] =
    Cogen[(
      GroupConstraintWaiver.Id,
      Group.Id,
      GroupConstraintViolation.Waivable,
      Attribution,
      Option[Attribution]
    )].contramap: w =>
      (w.id, w.groupId, w.violation, w.granted, w.revoked)

object ArbGroupConstraintWaiver extends ArbGroupConstraintWaiver
