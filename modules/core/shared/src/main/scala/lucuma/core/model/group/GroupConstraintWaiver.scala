// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.Eq
import lucuma.core.model.Attribution
import lucuma.core.model.Group
import lucuma.core.refined.auto.*
import lucuma.core.util.WithGid

/**
 * Acceptance of one violation of a group's constraints
 * (`GroupConstraintViolation`), say because the PI judged data taken at the
 * wrong time usable.  A waived violation no longer counts against the group,
 * while the constraint itself stays as written, so that any other violation
 * is still caught.
 *
 * Only a waivable violation can be waived (`GroupConstraintViolation.Waivable`).
 * A waiver is granted only for a violation that has happened.  `granted` records
 * who granted it and when, and why if they said.  A revoked waiver is kept,
 * with `revoked` recording the same for its revocation, but no longer applies.
 */
final case class GroupConstraintWaiver(
  id:        GroupConstraintWaiver.Id,
  groupId:   Group.Id,
  violation: GroupConstraintViolation.Waivable,
  granted:   Attribution,
  revoked:   Option[Attribution]
):

  /** Whether the waiver applies: it hasn't been revoked. */
  def isActive: Boolean =
    revoked.isEmpty

object GroupConstraintWaiver extends WithGid('w'.refined):

  given Eq[GroupConstraintWaiver] =
    Eq.by(w => (w.id, w.groupId, w.violation, w.granted, w.revoked))
