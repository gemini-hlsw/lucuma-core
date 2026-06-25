// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.Declination

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

final class DeclinationJS(dec: Declination):
  @JSExport val dms: String                = Declination.fromStringSignedDMS.reverseGet(dec).dropRight(3)
  @JSExport val degrees: Double            = dec.toAngle.toSignedDoubleDegrees
  @JSExport val microarcseconds: js.BigInt = js.BigInt(dec.toAngle.toMicroarcseconds.toString())
