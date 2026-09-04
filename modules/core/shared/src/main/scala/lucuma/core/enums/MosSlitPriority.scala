// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Placement priority of an object in a MOS mask design.
 *
 * When two objects' spectra would overlap, the higher priority object wins the slit. Mask design
 * software places acquisition objects first, then `High`, `Medium` and `Low` in turn; `Ignore`
 * marks an object to be excluded from the design entirely.
 *
 * Tags are the single characters used to encode a priority in a MOS mask file.
 *
 * '''The declaration order below is significant.''' `Enumerated` derives `Order` from it, and that
 * order is the placement order used by mask design software. Do not reorder these cases.
 *
 * @group Enumerations
 */
enum MosSlitPriority(
  val tag:      String,
  val longName: String
) derives Enumerated:

  /** Acquisition star, used to align the mask on sky rather than to take a spectrum. */
  case Acquisition extends MosSlitPriority("0", "Acquisition")

  /** Highest science priority. */
  case High        extends MosSlitPriority("1", "High")

  /** Medium science priority. */
  case Medium      extends MosSlitPriority("2", "Medium")

  /** Lowest science priority. */
  case Low         extends MosSlitPriority("3", "Low")

  /** Not a priority level: marks the object as excluded from the design. Sorts last. */
  case Ignore      extends MosSlitPriority("X", "Ignore")

object MosSlitPriority:

  given Display[MosSlitPriority] =
    Display.by(_.tag, _.longName)
