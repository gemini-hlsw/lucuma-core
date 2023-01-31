// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.arb.*
import lucuma.core.math.BrightnessValue
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen

trait ArbBrightnessValue:
  given Arbitrary[BrightnessValue] = newTypeArbitrary(BrightnessValue)
  given Cogen[BrightnessValue] = newTypeCogen(BrightnessValue)

object ArbBrightnessValue extends ArbBrightnessValue