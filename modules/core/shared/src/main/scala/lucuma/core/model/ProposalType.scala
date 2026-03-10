// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.TooActivation
import lucuma.core.util.TimeSpan

enum ProposalType {
  case Classical(minPercentTime: IntPercent, partnerSplits: List[PartnerSplit])
  case DemoScience(toOActivation: TooActivation, minPercentTime: IntPercent)
  case DirectorsTime(toOActivation: TooActivation, minPercentTime: IntPercent)
  case FastTurnaround(toOActivation: TooActivation, minPercentTime: IntPercent, reviewerId: ProgramUser.Id, mentorId: ProgramUser.Id)
  case LargeProgram(toOActivation: TooActivation, minPercentTime: IntPercent, minPercentTotalTime: IntPercent, totalTime: TimeSpan)
  case PoorWeather
  case Queue(toOActivation: TooActivation, minPercentTime: IntPercent, partnerSplits: List[PartnerSplit])
  case SystemVerification(toOActivation: TooActivation, minPercentTime: IntPercent)
}
