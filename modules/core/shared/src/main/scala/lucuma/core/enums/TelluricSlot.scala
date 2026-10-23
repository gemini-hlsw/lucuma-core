// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum TelluricSlot(val tag: String) derives Enumerated:
  case Before extends TelluricSlot("before")
  case After  extends TelluricSlot("after")

object TelluricSlot:
  def fromString(s: String): Option[TelluricSlot] =
    s.toLowerCase match
      case "before" => Some(Before)
      case "after"  => Some(After)
      case _        => None
