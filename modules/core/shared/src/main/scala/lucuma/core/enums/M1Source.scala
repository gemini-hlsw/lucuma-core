// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/** Enumerated type for M1 Source. */
enum M1Source(val tag: String) derives Enumerated:
  case PWFS1 extends M1Source("Pwfs1")
  case PWFS2 extends M1Source("Pwfs2")
  case OIWFS extends M1Source("Oiwfs")
  case GAOS  extends M1Source("Gaos")
  case HRWFS extends M1Source("Hrwfs")
