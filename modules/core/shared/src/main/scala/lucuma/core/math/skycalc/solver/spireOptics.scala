// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import gsp.math.optics.SplitMono

import spire.math.Number
import monocle.Iso

object spireOptics {
  val intNumber: SplitMono[Int, Number] = SplitMono(Number.apply, _.intValue)
  val intNumberIso: Iso[Int, Number]    = Iso((i: Int) => Number(i))(_.intValue)
}
