// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

/**
 * The GOA endpoint a query targets: the JSON API or the browsable search page for the same
 * selection.
 */
enum GoaEndpoint(val path: String):
  case JsonSummary extends GoaEndpoint("jsonsummary")
  case SearchForm  extends GoaEndpoint("searchform")
