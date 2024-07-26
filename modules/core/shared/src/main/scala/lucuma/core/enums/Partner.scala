// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum Partner(
  val tag: String,
  val shortName: String,
  val longName: String,
  val sites: Set[Site],
  val timeAccountingCategory: TimeAccountingCategory
) derives Enumerated {

  def abbreviation: String =
    tag.toUpperCase

  case AR extends Partner("ar", "Argentina", "Argentina",            Site.all.toSet, TimeAccountingCategory.AR)
  case BR extends Partner("br", "Brazil",    "Brazil",               Site.all.toSet, TimeAccountingCategory.BR)
  case CA extends Partner("ca", "Canada",    "Canada",               Site.all.toSet, TimeAccountingCategory.CA)
  case CL extends Partner("cl", "Chile",     "Chile",                Set(Site.GS),   TimeAccountingCategory.CL)
  case KR extends Partner("kr", "Korea",     "Republic of Korea",    Site.all.toSet, TimeAccountingCategory.KR)
  case UH extends Partner("uh", "U of H",    "University of Hawaii", Set(Site.GN),   TimeAccountingCategory.UH)
  case US extends Partner("us", "USA",       "United States",        Site.all.toSet, TimeAccountingCategory.US)

}