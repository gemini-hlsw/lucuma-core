// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.Declination

import scala.scalajs.js

final class DeclinationJS(dec: Declination) extends js.Object:
  val dms: String                = Declination.fromStringSignedDMS.reverseGet(dec).dropRight(4)
  val degrees: Double            = dec.toAngle.toSignedDoubleDegrees
  val microarcseconds: js.BigInt = js.BigInt(dec.toAngle.toMicroarcseconds.toString())
