// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Target-of-Opportunity activation, in order of increasing disruption.  Used
 * both as a proposal-level ceiling (the maximum an observation in the program
 * may reach) and as the observation's own value.
 *
 * An observation does not declare its activation. It is derived from whether the
 * observation is a Target of Opportunity -- that is, whether its asterism holds
 * an opportunity target -- together with its [[SchedulingMode]]; see
 * [[TooActivation.fromSchedulingMode]].
 */
enum TooActivation(val tag: String, val label: String) derives Enumerated:

  /** Not a Target of Opportunity. */
  case None extends TooActivation("none", "None")

  /** Observed whenever convenient, like any other observation. */
  case Standard extends TooActivation("standard", "Standard")

  /** Observed as soon as possible, but does not displace ongoing work. */
  case Rapid extends TooActivation("rapid", "Rapid")

  /** Observed as soon as possible, displacing ongoing work where permitted. */
  case Interrupting extends TooActivation("interrupting", "Interrupting")

object TooActivation:

  /**
   * Which kind of Target of Opportunity an observation scheduled in `mode` is.
   *
   * This assumes the observation is one -- that its asterism holds an
   * opportunity target -- and answers only which kind; asking is what says so.
   * An observation without an opportunity target has activation `None` whatever
   * its mode, and the caller answers that before reaching here.
   */
  def fromSchedulingMode(mode: SchedulingMode): TooActivation =
    mode match
      case SchedulingMode.Unconstrained | SchedulingMode.NoSplitting => TooActivation.Standard
      case SchedulingMode.Uninterruptible                            => TooActivation.Rapid
      case SchedulingMode.Interrupting                               => TooActivation.Interrupting
