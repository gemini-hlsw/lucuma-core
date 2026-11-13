// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Group
import lucuma.core.model.User
import lucuma.core.refined.auto.*
import lucuma.core.util.Timestamp
import lucuma.core.util.WithGid

/**
 * Acceptance of one violation of a group's constraints
 * (`GroupConstraintViolation`), say because the PI judged data taken at the
 * wrong time usable.  A waived violation no longer counts against the group,
 * while the constraint itself stays as written, so that any other violation
 * is still caught.
 *
 * Only a waivable violation can be waived (`GroupConstraintViolation.Waivable`).
 * A waiver is granted only for a violation that has happened, and records who
 * granted it, when and why.  A revoked waiver is kept, with who revoked it and
 * when, but no longer applies.
 */
final case class GroupConstraintWaiver(
  id:        GroupConstraintWaiver.Id,
  groupId:   Group.Id,
  violation: GroupConstraintViolation.Waivable,
  reason:    NonEmptyString,
  grantedBy: User.Id,
  grantedAt: Timestamp,
  revoked:   Option[GroupConstraintWaiver.Revocation]
):

  /** Whether the waiver applies: it hasn't been revoked. */
  def isActive: Boolean =
    revoked.isEmpty

object GroupConstraintWaiver extends WithGid('w'.refined):

  /** Who revoked a waiver, and when. */
  final case class Revocation(
    revokedBy: User.Id,
    revokedAt: Timestamp
  )

  object Revocation:
    given Eq[Revocation] = Eq.by(r => (r.revokedBy, r.revokedAt))

  given Eq[GroupConstraintWaiver] =
    Eq.by(w => (w.id, w.groupId, w.violation, w.reason, w.grantedBy, w.grantedAt, w.revoked))
