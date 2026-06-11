// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.ProperMotion

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

final class ProperMotionJS(_ra: ProperMotion.RA, _dec: ProperMotion.Dec):
  @JSExport
  val dec: ProperMotionDeclinationJS = ProperMotionDeclinationJS(_dec)
  @JSExport
  val ra: ProperMotionRAJS           = ProperMotionRAJS(_ra)

final class ProperMotionDeclinationJS(dec: ProperMotion.Dec):
  @JSExport val microarcsecondsPerYear: js.BigInt =
    js.BigInt(ProperMotion.Dec.microarcsecondsPerYear.reverseGet(dec).toString)
  @JSExport val milliarcsecondsPerYear: Double    =
    ProperMotion.Dec.milliarcsecondsPerYear.get(dec).doubleValue

final class ProperMotionRAJS(ra: ProperMotion.RA):
  @JSExport val microarcsecondsPerYear: js.BigInt =
    js.BigInt(ProperMotion.RA.microarcsecondsPerYear.reverseGet(ra).toString)
  @JSExport val milliarcsecondsPerYear: Double    =
    ProperMotion.RA.milliarcsecondsPerYear.get(ra).doubleValue
