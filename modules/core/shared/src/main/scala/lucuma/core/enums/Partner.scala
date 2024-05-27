// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum Partner(
  val tag: String,
  val shortName: String,
  val longName: String,
  val sites: Set[Site]
) derives Enumerated {

  def abbreviation: String =
    tag.toUpperCase

  case AR extends Partner("ar", "Argentina", "Argentina",            Site.all.toSet)
  case BR extends Partner("br", "Brazil",    "Brazil",               Site.all.toSet)
  case CA extends Partner("ca", "Canada",    "Canada",               Site.all.toSet)
  case CL extends Partner("cl", "Chile",     "Chile",                Set(Site.GS))
  case KR extends Partner("kr", "Korea",     "Republic of Korea",    Site.all.toSet)
  case UH extends Partner("uh", "U of H",    "University of Hawaii", Set(Site.GN))
  case US extends Partner("us", "USA",       "United States",        Site.all.toSet)

}