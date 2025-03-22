// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import lucuma.core.util.Enumerated

enum AgsGuideQuality(private val tag: String, val message: String) derives Enumerated:
  case DeliversRequestedIq
      extends AgsGuideQuality("delivers_requested_id", "Delivers requested IQ.")

  case PossibleIqDegradation
      extends AgsGuideQuality("possible_iq_degradation",
                              "Slower guiding required; may not deliver requested IQ."
      )

  case IqDegradation
      extends AgsGuideQuality("iq_degradation",
                              "Slower guiding required; will not deliver requested IQ."
      )

  case PossiblyUnusable extends AgsGuideQuality("possible_unusable", "May not be able to guide.")

  case Unusable extends AgsGuideQuality("unusable", "Unable to guide.")
