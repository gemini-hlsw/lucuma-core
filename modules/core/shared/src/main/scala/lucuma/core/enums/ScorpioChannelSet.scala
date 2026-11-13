// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * SCORPIO channels used for science in an observation. The inactive arm is idled by the
 * instrument software (IR arrays reset, VIS shutters closed).
 * @group Enumerations
 */
enum ScorpioChannelSet(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val channels:  NonEmptyList[ScorpioChannel]
) derives Enumerated, Display:
  case Both    extends ScorpioChannelSet("both",    "VIS+IR", "Visible and Infrared", NonEmptyList.of(ScorpioChannel.Vis, ScorpioChannel.Ir))
  case VisOnly extends ScorpioChannelSet("vis_only", "VIS",    "Visible only",         NonEmptyList.one(ScorpioChannel.Vis))
  case IrOnly  extends ScorpioChannelSet("ir_only",  "IR",     "Infrared only",        NonEmptyList.one(ScorpioChannel.Ir))
