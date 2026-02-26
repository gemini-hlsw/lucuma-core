// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.ToOActivation
import lucuma.core.util.TimeSpan

enum ProposalType {
  case Classical(minPercentTime: IntPercent, partnerSplits: List[PartnerSplit])
  case DemoScience(toOActivation: ToOActivation, minPercentTime: IntPercent)
  case DirectorsTime(toOActivation: ToOActivation, minPercentTime: IntPercent)
  case FastTurnaround(toOActivation: ToOActivation, minPercentTime: IntPercent, reviewerId: ProgramUser.Id, mentorId: ProgramUser.Id)
  case LargeProgram(toOActivation: ToOActivation, minPercentTime: IntPercent, minPercentTotalTime: IntPercent, totalTime: TimeSpan)
  case PoorWeather
  case Queue(toOActivation: ToOActivation, minPercentTime: IntPercent, partnerSplits: List[PartnerSplit])
  case SystemVerification(toOActivation: ToOActivation, minPercentTime: IntPercent)
}
