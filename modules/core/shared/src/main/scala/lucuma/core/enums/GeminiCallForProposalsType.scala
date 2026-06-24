// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

enum GeminiCallForProposalsType(val tag: String, val title: String) derives Enumerated:
  case DemoScience        extends GeminiCallForProposalsType("demo_science", "Demo Science")
  case DirectorsTime      extends GeminiCallForProposalsType("directors_time", "Director's Time")
  case FastTurnaround     extends GeminiCallForProposalsType("fast_turnaround", "Fast Turnaround")
  case LargeProgram       extends GeminiCallForProposalsType("large_program", "Large Program")
  case PoorWeather        extends GeminiCallForProposalsType("poor_weather", "Poor Weather")
  case RegularSemester    extends GeminiCallForProposalsType("regular_semester", "Regular Semester")
  case SystemVerification extends GeminiCallForProposalsType("system_verification", "System Verification")

  def subTypes: NonEmptyList[ScienceSubtype] =
      this match
        case LargeProgram       => NonEmptyList.of(ScienceSubtype.LargeProgram)
        case FastTurnaround     => NonEmptyList.of(ScienceSubtype.FastTurnaround)
        case RegularSemester    => NonEmptyList.of(ScienceSubtype.Classical, ScienceSubtype.Queue)
        case SystemVerification => NonEmptyList.of(ScienceSubtype.SystemVerification)
        case PoorWeather        => NonEmptyList.of(ScienceSubtype.PoorWeather)
        case DemoScience        => NonEmptyList.of(ScienceSubtype.DemoScience)
        case DirectorsTime      => NonEmptyList.of(ScienceSubtype.DirectorsTime)
