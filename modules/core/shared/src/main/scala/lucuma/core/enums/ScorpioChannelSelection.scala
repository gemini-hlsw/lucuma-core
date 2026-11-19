// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * SCORPIO arms used for science in an observation.
 * @group Enumerations
 */
enum ScorpioChannelSelection(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val arms:      NonEmptyList[ScorpioArm]
) derives Enumerated, Display:
  case Both    extends ScorpioChannelSelection("vis_ir",   "VIS+IR", "Visible and Infrared", NonEmptyList.of(ScorpioArm.Vis, ScorpioArm.Ir))
  case VisOnly extends ScorpioChannelSelection("vis_only", "VIS",    "Visible only",         NonEmptyList.one(ScorpioArm.Vis))
  case IrOnly  extends ScorpioChannelSelection("ir_only",  "IR",     "Infrared only",        NonEmptyList.one(ScorpioArm.Ir))
