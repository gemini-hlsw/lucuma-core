// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

enum CallForProposalsType(val tag: String, val title: String) derives Enumerated:
  case DemoScience        extends CallForProposalsType("demo_science", "Demo Science")
  case DirectorsTime      extends CallForProposalsType("directors_time", "Director's Time")
  case FastTurnaround     extends CallForProposalsType("fast_turnaround", "Fast Turnaround")
  case LargeProgram       extends CallForProposalsType("large_program", "Large Program")
  case PoorWeather        extends CallForProposalsType("poor_weather", "Poor Weather")
  case RegularSemester    extends CallForProposalsType("regular_semester", "Regular Semester")
  case SystemVerification extends CallForProposalsType("system_verification", "System Verification")

  def subTypes: NonEmptyList[ScienceSubtype] =
      this match
        case LargeProgram       => NonEmptyList.of(ScienceSubtype.LargeProgram)
        case FastTurnaround     => NonEmptyList.of(ScienceSubtype.FastTurnaround)
        case RegularSemester    => NonEmptyList.of(ScienceSubtype.Classical, ScienceSubtype.Queue)
        case SystemVerification => NonEmptyList.of(ScienceSubtype.SystemVerification)
        case PoorWeather        => NonEmptyList.of(ScienceSubtype.PoorWeather)
        case DemoScience        => NonEmptyList.of(ScienceSubtype.DemoScience)
        case DirectorsTime      => NonEmptyList.of(ScienceSubtype.DirectorsTime)
