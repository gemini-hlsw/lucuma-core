// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.TooActivation
import lucuma.core.util.TimeSpan
import monocle.Optional

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

object ProposalType:
  
  val TooActivation: Optional[ProposalType, TooActivation] =
    Optional[ProposalType, TooActivation] {
      case Classical(_, _)                        => None
      case DemoScience(toOActivation, _)          => Some(toOActivation)
      case DirectorsTime(toOActivation, _)        => Some(toOActivation)
      case FastTurnaround(toOActivation, _, _, _) => Some(toOActivation)
      case LargeProgram(toOActivation, _, _, _)   => Some(toOActivation)
      case PoorWeather                            => None
      case Queue(toOActivation, _, _)             => Some(toOActivation)
      case SystemVerification(toOActivation, _)   => Some(toOActivation)
    } { too => {
        case t @ Classical(_, _)            => t
        case t @ DemoScience(_, _)          => t.copy(toOActivation = too)
        case t @ DirectorsTime(_, _)        => t.copy(toOActivation = too)
        case t @ FastTurnaround(_, _, _, _) => t.copy(toOActivation = too)
        case t @ LargeProgram(_, _, _, _)   => t.copy(toOActivation = too)
        case t @ PoorWeather                => t
        case t @ Queue(_, _, _)             => t.copy(toOActivation = too)
        case t @ SystemVerification(_, _)   => t.copy(toOActivation = too)
      }
    }