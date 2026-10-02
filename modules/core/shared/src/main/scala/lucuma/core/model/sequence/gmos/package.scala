// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * GMOS exposure time limits (Hamamatsu detectors).  The detector controller
 * accepts only whole seconds.  Longer than `MaxWarningExposureTime` is
 * legal, but cosmic ray contamination becomes excessive.
 */
val MinExposureTime: TimeSpan        = 1.secTimeSpan
val MaxWarningExposureTime: TimeSpan = 1200.secTimeSpan
