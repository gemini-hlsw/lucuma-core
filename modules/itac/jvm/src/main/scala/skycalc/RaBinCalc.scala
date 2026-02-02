// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import lucuma.core.enums.Site

import java.time.ZonedDateTime

trait RaBinCalc:
  def calc(site: Site, start: ZonedDateTime, end: ZonedDateTime, size: RaBinSize): List[Hours]
