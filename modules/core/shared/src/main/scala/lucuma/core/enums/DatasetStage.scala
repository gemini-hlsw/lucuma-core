// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum DatasetStage(val tag: String, val name: String, val description: String) derives Enumerated {

  case EndExpose    extends DatasetStage("end_expose",    "End Expose",    "Marks the end of photon collection.")
  case EndReadout   extends DatasetStage("end_readout",   "End Readout",   "Marks the end of detector readout.")
  case EndWrite     extends DatasetStage("end_write",     "End Write",     "Marks the end of write to permanent store.")
  case StartExpose  extends DatasetStage("start_expose",  "Start Expose",  "Marks the start of photon collection.")
  case StartReadout extends DatasetStage("start_readout", "Start Readout", "Marks the start of detector readout.")
  case StartWrite   extends DatasetStage("start_write",   "Start Write",   "Marks the start of write to permanent store.")

}