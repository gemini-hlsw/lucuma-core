// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group
package arb

import eu.timepit.refined.scalacheck.string.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Group
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

import GroupConstraintWaiver.Revocation

trait ArbGroupConstraintWaiver:
  import ArbGid.given
  import ArbGroupConstraintViolation.given
  import ArbTimestamp.given

  given Arbitrary[Revocation] =
    Arbitrary:
      for
        u <- arbitrary[User.Id]
        t <- arbitrary[Timestamp]
      yield Revocation(u, t)

  given Cogen[Revocation] =
    Cogen[(User.Id, Timestamp)].contramap(r => (r.revokedBy, r.revokedAt))

  given Arbitrary[GroupConstraintWaiver] =
    Arbitrary:
      for
        id <- arbitrary[GroupConstraintWaiver.Id]
        g  <- arbitrary[Group.Id]
        m  <- arbitrary[GroupConstraintViolation.Waivable]
        r  <- arbitrary[NonEmptyString]
        u  <- arbitrary[User.Id]
        t  <- arbitrary[Timestamp]
        v  <- arbitrary[Option[Revocation]]
      yield GroupConstraintWaiver(id, g, m, r, u, t, v)

  given Cogen[GroupConstraintWaiver] =
    Cogen[(
      GroupConstraintWaiver.Id,
      Group.Id,
      GroupConstraintViolation.Waivable,
      String,
      User.Id,
      Timestamp,
      Option[Revocation]
    )].contramap: w =>
      (w.id, w.groupId, w.violation, w.reason.value, w.grantedBy, w.grantedAt, w.revoked)

object ArbGroupConstraintWaiver extends ArbGroupConstraintWaiver
