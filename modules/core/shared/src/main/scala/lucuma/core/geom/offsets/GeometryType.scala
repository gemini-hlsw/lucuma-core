// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.offsets

import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.util.Enumerated

enum GeometryType(private val tag: String, val isSequence: Boolean, val isGuided: Boolean)
    derives Enumerated:
  case BlindOffset       extends GeometryType("blind_offset", false, false)
  case AcqGuidedOffset   extends GeometryType("acq_guided_offset", true, true)
  case AcqUnguidedOffset extends GeometryType("acq_unguided_offset", true, false)
  case SciGuidedOffset   extends GeometryType("sci_guided_offset", true, true)
  case SciUnguidedOffset extends GeometryType("sci_unguided_offset", true, false)
  case Base              extends GeometryType("base", false, false)
  case Intersection      extends GeometryType("intersection", false, false)
  case Vignetting        extends GeometryType("vignetting", false, false)

object GeometryType:
  def fromSequenceTypeAndGuiding(
    sequenceType: SequenceType,
    guiding:      StepGuideState
  ): GeometryType =
    (sequenceType, guiding) match
      case (SequenceType.Acquisition, StepGuideState.Enabled)  => GeometryType.AcqGuidedOffset
      case (SequenceType.Acquisition, StepGuideState.Disabled) => GeometryType.AcqUnguidedOffset
      case (SequenceType.Science, StepGuideState.Enabled)      => GeometryType.SciGuidedOffset
      case (SequenceType.Science, StepGuideState.Disabled)     => GeometryType.SciUnguidedOffset
