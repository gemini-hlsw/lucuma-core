// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Timestamp

/**
 * Who took an action, when, and why, if they said: say, granting or revoking a
 * waiver.
 */
final case class Attribution(
  by:     User.Id,
  at:     Timestamp,
  reason: Option[NonEmptyString]
)

object Attribution:

  given Eq[Attribution] =
    Eq.by(a => (a.by, a.at, a.reason))
