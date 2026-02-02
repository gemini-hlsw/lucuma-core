// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import scala.annotation.unused

trait RaBinSize(@unused min: Int):
  def getBinCount: Int = ???
