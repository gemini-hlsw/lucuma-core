// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import coulomb.Quantity
import coulomb.define.BaseUnit
import coulomb.integrations.cats.quantity.given
import eu.timepit.refined.numeric.Interval
import lucuma.core.refined.cats.given
import lucuma.core.util.NewRefined
import monocle.Focus
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

final type GnirsFocusMotorStep
given BaseUnit[GnirsFocusMotorStep, "motor step", "step"] = BaseUnit()

type GnirsFocusMotorStepRefinement = Interval.Closed[-179_999, 180_000]
object GnirsFocusMotorStepsValue extends NewRefined[Int, GnirsFocusMotorStepRefinement]
type GnirsFocusMotorStepsValue = GnirsFocusMotorStepsValue.Type

enum GnirsFocus derives Eq:
  case Best
  case Custom(value: Quantity[GnirsFocusMotorStepsValue, GnirsFocusMotorStep])

object GnirsFocus:
  val best: Prism[GnirsFocus, GnirsFocus.Best.type] = GenPrism[GnirsFocus, GnirsFocus.Best.type]
  val custom: Prism[GnirsFocus, GnirsFocus.Custom] = GenPrism[GnirsFocus, GnirsFocus.Custom]

  object Custom:
    val value: Iso[GnirsFocus.Custom, Quantity[GnirsFocusMotorStepsValue, GnirsFocusMotorStep]] =
      Focus[GnirsFocus.Custom](_.value)
