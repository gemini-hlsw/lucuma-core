// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbCalibrationEstimate.given
import lucuma.core.model.sequence.arb.ArbCategorizedTime.given
import monocle.law.discipline.*
import munit.*

class CalibrationEstimateSuite extends DisciplineSuite:
  checkAll("Eq[CalibrationEstimate]",     EqTests[CalibrationEstimate].eqv)
  checkAll("Monoid[CalibrationEstimate]", MonoidTests[CalibrationEstimate].monoid)
  checkAll("CalibrationEstimate.count",   LensTests(CalibrationEstimate.count))
  checkAll("CalibrationEstimate.time",    LensTests(CalibrationEstimate.time))
