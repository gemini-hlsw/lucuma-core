// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import monocle.Prism

/**
 * Placement priority of an object in a MOS mask design.
 *
 * When two objects' spectra would overlap, the higher priority object wins the slit. Mask design
 * software places acquisition objects first, then `High`, `Medium` and `Low` in turn; `Ignore`
 * marks an object to be excluded from the design entirely.
 *
 * '''The declaration order below is significant.''' `Enumerated` derives `Order` from it, and that
 * order is the placement order used by mask design software. Do not reorder these cases.
 *
 * @group Enumerations
 */
enum MosSlitPriority(
  val tag:       String,
  val fitsValue: Char,
  val shortName: String,
  val longName:  String
) derives Enumerated:

  /** Acquisition star, used to align the mask on sky rather than to take a spectrum. */
  case Acquisition extends MosSlitPriority("acquisition", '0', "0", "Acquisition")

  /** Highest science priority. */
  case High        extends MosSlitPriority("high",        '1', "1", "High")

  /** Medium science priority. This is the default when a file omits the column. */
  case Medium      extends MosSlitPriority("medium",      '2', "2", "Medium")

  /** Lowest science priority. */
  case Low         extends MosSlitPriority("low",         '3', "3", "Low")

  /** Not a priority level: marks the object as excluded from the design. Sorts last. */
  case Ignore      extends MosSlitPriority("ignore",      'X', "X", "Ignore")

object MosSlitPriority:

  /** The single character used to encode a priority in a MOS mask file. */
  val fromFitsValue: Prism[Char, MosSlitPriority] =
    Prism[Char, MosSlitPriority](c => values.find(_.fitsValue === c))(_.fitsValue)

  given Display[MosSlitPriority] =
    Display.by(_.shortName, _.longName)
