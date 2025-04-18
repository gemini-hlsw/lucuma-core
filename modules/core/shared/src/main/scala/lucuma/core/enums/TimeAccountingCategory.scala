// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Time accounting categories.  Time is allocated and time is charged to one or
 * more of these categories.  Each Gemini partner has its own time accounting
 * category which is used for queue, classical and fast turnaround proposals.
 * Other proposal types have their own distinct categories.
  */
enum TimeAccountingCategory(
  val tag: String,
  val description: String
) derives Enumerated:

  def abbreviation: String = tag.toUpperCase

  case AR   extends TimeAccountingCategory("ar",   "Argentina")
  case BR   extends TimeAccountingCategory("br",   "Brazil")
  case CA   extends TimeAccountingCategory("ca",   "Canada")
  case CFHT extends TimeAccountingCategory("cfht", "CFHT Exchange")
  case CL   extends TimeAccountingCategory("cl",   "Chile")
  case DD   extends TimeAccountingCategory("dd",   "Director's Time")
  case DS   extends TimeAccountingCategory("ds",   "Demo Science")
  case GT   extends TimeAccountingCategory("gt",   "Guaranteed Time")
  case JP   extends TimeAccountingCategory("jp",   "Subaru")
  case KECK extends TimeAccountingCategory("keck", "Keck Exchange")
  case KR   extends TimeAccountingCategory("kr",   "Republic of Korea")
  case LP   extends TimeAccountingCategory("lp",   "Large Program")
  case LTP  extends TimeAccountingCategory("ltp",  "Limited-term Participant")
  case SV   extends TimeAccountingCategory("sv",   "System Verification")
  case UH   extends TimeAccountingCategory("uh",   "University of Hawaii")
  case US   extends TimeAccountingCategory("us",   "United States")

end TimeAccountingCategory