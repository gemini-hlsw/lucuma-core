// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/** Altair (Gemini North adaptive optics) guiding modes. */
enum AltairMode(val tag: String, val name: String) derives Enumerated, Display:

  /** Natural guide star drives both high order correction and tip/tilt. */
  case Ngs extends AltairMode("ngs", "NGS")

  /** Laser drives high order correction, a natural star on the AOWFS provides tip/tilt/focus. */
  case Lgs extends AltairMode("lgs", "LGS")

  /** Laser drives high order correction, a natural star on PWFS1 provides tip/tilt/focus. */
  case LgsP1 extends AltairMode("lgs_p1", "LGS+P1")

  def usesLaser: Boolean = this match
    case Ngs         => false
    case Lgs | LgsP1 => true
