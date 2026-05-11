// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq
import cats.syntax.eq.*
import lucuma.core.model.Target


/**
 * A `GhostTarget` is limited to sidereal or non-sidereal.
 */
type GhostTarget = Target.Sidereal | Target.Nonsidereal

given Eq[GhostTarget] =
  Eq.instance:
    case (s0: Target.Sidereal,    s1: Target.Sidereal)    => s0 === s1
    case (n0: Target.Nonsidereal, n1: Target.Nonsidereal) => n0 === n1
    case _                                                => false